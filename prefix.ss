(import :drewc/smug/primitive :drewc/smug/simple)
(export #t)

(def (smug:map p (using +++)) (many p +++))

(def (smug:or p . ps) (+++ p (if (null? ps) (fail) (apply smug:or ps))))

(def (smug:and p . ps)
  (bind p (lambda (v) (if (null? ps) (return v) (apply smug:and ps)))))

(def (smug:not p)
  (bind (return #t)
        (lambda (not)
          (+++ (bind p (lambda _ (set! not #f) (fail)))
               (bind (return #!void) (lambda _ (if not (return not) (fail))))))))

(def (smug:peek-char (p (item)))
  (def EOF (bind (smug:not (item)) (lambda _ (return #!eof))))
  (bind (return #!void)
        (lambda (peek) (smug:or (bind (+++ p EOF)
                                 (lambda (c) (set! peek c) (fail)))
                           (bind (return #!void)
                                 (lambda _ (if (void? peek)
                                        (fail)
                                        (return peek))))))))

(def (smug:begin0 parser . parsers)
  (smug:let* ((result parser)
              (_ (if (null? parsers) (return result) (apply smug:and parsers))))
    (return result)))


(def (smug:char=? char (pred char=?))
  (satisfies (cut pred <> char)))

(def (smug:read-line eof-fail?: (eof-fail? #f)
                     map: (f list->string))
  (smug:let*
   ((chars (many (satisfies (lambda (c) (not (char=? #\newline c)))) +++))
    (nl (smug:or (item) (return #!eof))))
   ;; If there are no chars and we are EOF, we fail
   ;; If EOF does not define a line, and we're EOF, we fail
   (if (and (eof-object? nl) (or (null? chars) eof-fail?))
     (fail)
     (return (f chars)))))

(def (smug:string=? str start: (start 0) end: (end (string-length str))
                   predicate: (p? char=?)
                   return-parsed?: (r? #f))
 (if (= start end)
    (return [])
    (smug:let* ((x (satisfies (cut p? (string-ref str start) <>)))
                (xs (smug:string=? str start: (+ 1 start) end: end
                                  predicate: p?
                                  return-parsed?: r?)))
               (return (if r? (cons x xs) str)))))

(def (smug:string-ci=? str . args)
  (apply smug:string=? str predicate: char-ci=? args))
