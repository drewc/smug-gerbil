(import :drewc/smug :std/srfi/13 :std/srfi/1 :std/iter)
(export #t)

(def <ws> (skip (satisfies (lambda (c) (and (char-whitespace? c) (not (char=? c #\newline))))))) 

(def <end-keyword> (smug:peek-char (smug:or (satisfies char-whitespace?) (smug:not (item)))))

(def (<keyword> name)
  (def <header> (smug:or (smug:and <ws> (smug:read-line)) (return "")))
  (smug:and <ws> (smug:string-ci=? (string-append "#+" name)) <end-keyword> <header>))

(def <name> (smug:or (bind (<keyword> "name:") (lambda (l) (return (string-trim-right l)))) (return #f)))

(def <lines> (smug:or (smug:and (<keyword> "end_src") (return []))
                    (smug:let* ((x (smug:read-line))
                                (xs <lines>))
                      (return (cons x xs)))))

(def <code-block-plist>
    (smug:let* ((name <name>) (header (<keyword> "begin_src")) (lines <lines>))
      (return [name: name header: header lines: lines])))

(defstruct code-block
  (name language switches header-arguments body))

(def <language>
  ((liftP list->string) (smug:map (satisfies-not char-whitespace?))))

(def <switches>
  (let P ()
    (def <ws> (skip (satisfies char-whitespace?)))

    (def (<switch> (letter #\n) (arg (return [])))
      (smug:let* ((_ (smug:and <ws> (smug:char=? #\-) (smug:char=? letter)))
                  (arg arg))
        (return (cons (string->symbol (string letter)) (list->string arg)))))

    (def <n> (smug:map (satisfies char-numeric?)))
    (def <l> (let* ((char #\") (<q> (smug:char=? char))
                    (<backslash> (smug:and (smug:char=? #\\) (item)))
                    (<body> (smug:map (smug:or (satisfies-not (cut char=? char <>))
                                           <backslash>))))
             (smug:and <ws> <q> (smug:begin0 <body> <q>))))

    (smug:map (smug:or (<switch> #\n <n>) (<switch> #\r) (<switch> #\i) (<switch> #\l <l>)))))


(def <header-arguments>
  (let P ()
    (def <header-argument-name>
      ((liftP (lambda (lst) (string->symbol (list->string lst))))
       (smug:and (smug:char=? #\:)
                 (many (satisfies char-lower-case?) +++))))

    (def <header-argument-value>
      ((liftP list->string) (smug:map (satisfies-not (cut char=? #\: <>)))))

    (def <header-argument>
         (smug:let* ((name (smug:and (skip (satisfies char-whitespace?))
                                     <header-argument-name>))
                     (value (smug:and (skip (satisfies char-whitespace?))
                                      <header-argument-value>)))
           (return (cons name (string-trim-right value)))))

       (smug:map <header-argument>)))

(def <code-block>
  (let P ()
    (def <L.S.HA>
      (smug:let* ((lang <language>) (sws <switches>) (has <header-arguments>))
        (return (list lang sws has))))

    (def (getf name list)
      (cadr (member name list)))

    (bind <code-block-plist>
          (lambda (cb)
            (let ((name (getf name: cb))
                  (header (getf header: cb))
                  (body (getf lines: cb)))
              (match (run <L.S.HA> header)
                ([l s has] (return (make-code-block name l s has body)))))))))

(def (read-code-blocks inp)

  (def <skip-line> (bind (item) (lambda (x) (if (char=? #\newline x) (return #f) <skip-line>))))

  (def <code-block?> (smug:or <code-block> <skip-line>))

  (def String
    (if (not (input-port? inp)) inp 
        (list->string (let loop ((c (read-char inp))) 
                        (if (eof-object? c) [] (cons c (loop (read-char inp))))))))

  (filter identity (run (many <code-block?> +++) String)))

(def (code-block-tangle? cb)
  (let (arg (assoc 'tangle (code-block-header-arguments cb)))
    (when arg (set! arg (cdr arg)))
    (if arg (if (string=? "no" arg) #f #t) #f)))

(def (code-block-tangle-filename cb (default #t))
  (let (arg (assoc 'tangle (code-block-header-arguments cb)))
    (when arg (set! arg (cdr arg)))
    (if arg (if (string=? "yes" arg) default (if (string=? "no" arg) #f arg)) #f))) ;| for highlighting a silly thing.

(def org-src-preserve-indentation #f)

(def (code-block-lines-skip-n cb)
  (def <ws-count>
    (bind (smug:map (satisfies char-whitespace?))
          (lambda (lst) (return (length lst)))))

  (if (or org-src-preserve-indentation
          (assoc 'i (code-block-switches cb)))
    0
    (apply min (map (cut run <ws-count> <>)
                    (filter (? (not string-null?)) (code-block-body cb))))))

(def (strip-comma line)
 (if (string-null? line) line 
  (run (smug:or (smug:and (smug:char=? #\,)
                 (smug:or (smug:char=? #\*)
                          (smug:string=? ",*")
                          (smug:string=? "#+")
                          (smug:string=? ",#+"))
                 (return (string-drop line 1)))
                (return line))
       line))) 

(defstruct noweb-ref (name))

(def <noweb-ref>
  (let P ()
    (def (<char> prev)
      (smug:or (satisfies (? (not (cut char=? #\> <>))))
               (smug:begin0 (smug:char=? #\>)
                            (smug:not (smug:char=? #\>)))
               (if (or (char-whitespace? prev)
                       (char=? #\> prev))
                 (item)
                 (fail))))

    (def (<chars> (p #\null))
      (smug:let* ((x (<char> p))
                  (xs (smug:or (<chars> x) (return []))))
                 (return (cons x xs))))

    (smug:and (smug:string=? "<<")
              (smug:not (satisfies char-whitespace?))
              (smug:begin0 ((liftP make-noweb-ref)
                            ((liftP list->string)
                             (<chars>)))
                           (smug:string=? ">>")))))

(defstruct noweb-line (prefix ref postfix))

(def <noweb-line>
  (let p ()
    (def (partition-line lst)
      (let ((pre (take-while char? lst)))
        (match (find-tail noweb-ref? lst)
          ([ref . post]
           (make-noweb-line (list->string pre) ref (list->string post)))
          (otherwise (error "Cannot partition noweb line " lst otherwise)))))
    (bind (smug:map (smug:or <noweb-ref>
                             (item)))
          (lambda (l)
            (if (null? (filter noweb-ref? l))
              (fail)
              (return (partition-line l)))))))

(def (expand-noweb-lines? cb)
  (def exp? (assoc 'noweb (code-block-header-arguments cb)))
  (if (not exp?) #f
      (any (cut equal? (cdr exp?) <>)
           '("yes" "tangle" "no-export" "strip-export" "eval"))))

(def (code-block-lines cb blocks)

  (def n (code-block-lines-skip-n cb))

  (def (line-trim-left l)
    (if (or (zero? n) (string-null? l)) l (strip-comma (string-drop l n))))

  (def (parser line)
    (run (smug:or  (if (expand-noweb-lines? cb) <noweb-line> (return line))
                   (return line))
         line))


  (def lines (map parser (map line-trim-left (code-block-body cb))))

  (if (expand-noweb-lines? cb)
    (append-map (lambda (l) (if (noweb-line? l)
                         (expand-noweb-line l blocks)
                         [l]))
                lines)
    lines))

(def (noweb-ref-code-block-lines noweb-ref list-of-code-blocks)
  (append-map (cut code-block-lines <> list-of-code-blocks)
              (filter (lambda (cb) (equal? (code-block-name cb) (noweb-ref-name noweb-ref)))
                      list-of-code-blocks)))

(def (expand-noweb-line noweb-line list-of-code-blocks)
  (let* ((ref (noweb-line-ref noweb-line))
        (lines (map (lambda (<>) (string-append (noweb-line-prefix noweb-line) <>))
                    (noweb-ref-code-block-lines ref list-of-code-blocks))))
    ;; (when (null? lines)
    ;;   (displayln "Cannot find lines:" (noweb-ref-name ref)))
    (begin0 lines
      (unless (null? lines)
        (set! (car (last-pair lines))
          (string-append (last lines) (noweb-line-postfix noweb-line)))))))
