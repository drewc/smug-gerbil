(import :drewc/smug/primitive
        (for-syntax :drewc/smug/primitive)
        (only-in :std/srfi/13 string-null?)
        :std/srfi/1)
(export #t)

;; sat p = item ‘bind‘ \x -> if p x then return x else fail
(def (sat predicate (p (item)))
    (bind p (lambda (x) (if (predicate x) (return x) (fail)))))

(def (satisfies predicate item: (item item))
  (bind (item) (lambda (x) (if (predicate x) (return x) (fail)))))

(def (skip p) (+++ (bind p (lambda _ (+++ (skip p) (return #t)))) (return #f)))

(def (liftP function . args) 
  (cut bind <> (lambda (v) (return (apply function v args)))))

(def (.char=? c) (sat (item) (cut char=? <> c)))
(def (.char-ci=? c) (sat (item) (cut char-ci=? <> c)))

(def (ci=? thing (ret #f))
 (if (string? thing) (.string-ci=? thing ret) (.char-ci=? thing)))

(def (peek (p (item)))
  (let (v (gensym))
    (.let* (peek (return v))
      (.or (.let* (x p) (set! peek x) (fail))
           (.let* (_ #f) (if (eq? peek v) (fail) (return peek)))))))

(def (.begin p . ps)
  (bind p (lambda (v) (if (null? ps) (return v) (apply .begin ps)))))

(def (.begin0 p . ps)
  (.let* ((x p) (_ (if (null? ps) (return ps) (apply .begin ps))))
    (return x)))



(def (.or p . ps) (+++ p (if (null? ps) (fail) (apply .or ps))))
(def (.any p . ps) (++ p (if (null? ps) (fail) (apply .any ps))))


(def (save-excursion . ps) (if (null? ps) (fail) (peek (apply .begin ps))))

(def (skip-chars-forward charbag (end #f))
  (def lst (if (list? charbag) charbag (string->list charbag)))
  (let sk ((ret #f))
    (.or (.let* (p (point))
           (if (and end (>= p end)) (return ret)
              (.begin (sat (cut memv <> lst)) (sk #t))))
         (return ret))))

(def (skip-chars-backward charbag (start #f))
  (def lst (if (list? charbag) charbag (string->list charbag)))
  (def (skb (p #f) (ret #f))
    (.or 
     (if (or (zero? p) (and start (<= start p))) (return ret)
         (.let* (bp (goto-char (1- p)))
            (.begin (sat (cut memv <> lst))
                    (skb bp #t))))
     (return ret)))
  (bind (point) skb))

(def (forward-line (count 1))
  (.begin (many (sat (? (not (cut char=? #\newline <>)))))
          #\newline
          (if (> count 1) (forward-line (1- count)) (point))))

(def (beginning-of-line (count 1))
  (def (bol p)
    (if (zero? p) (return p)
        (let ((bp (1- p)))
          (.let* (c (.begin (goto-char bp) (item)))
            (if (char=? #\newline c)
                    (return p)
                    (bol bp))))))
    (when (> count 1)
      (forward-line (1- count)))
  (bind (point) bol))

(def (buffer-substring start end)
  (peek (.begin (goto-char start) (.make-string (- end start)))))

(def (count-lines start end)
  (save-excursion (goto-char start)
                  (.let* (lst (.make-list (- end start) (item)))
                      (return (count (cut char=? #\newline <>) lst)))))




;; Some.

(def (some p)
  (lazy+ (return []) (.let* ((x p) (xs (some p))) (cons x xs))))

(def (some1 p) (.let* ((x p) (xs (some p))) (cons x xs)))



;; bracket open p close = [x | _ <- open, x <- p, _ <- close]

(def (bracket open p close) (.let* ((_ open) (x p) (_ close)) (return x)))


;; many p = [x:xs | x <- p, xs <- many p] ++ [[]]

(def (many parser (plus +++))
  (plus (.let* ((x parser) (xs (many parser plus))) (return [x . xs]))
        (return [])))

(def (many1 p (plus +++))
  (.let* ((x p) (xs (many p plus))) [x . xs]))

(def (at-least n parser (plus +++))
  (plus (.let* ((x parser)
                    (xs (at-least (- n 1) parser plus)))
         (return [x . xs]))
        (if (> n 0) (fail) (return []))))





;; sepby1:: Parser a -> Parser b -> Parser [a]
;; p ‘sepby1‘ sep = [x:xs | x <- p , xs <- many [y | _ <- sep , y <- p]]   

(def (sepby1 p sep (plus +++))
  (.let* ((x p) (xs (many (.let* ((_ sep) (y p)) (return y)) plus))) 
   (return [x . xs])))


(def (.cons p q) (.let* ((x p) (y q)) (cons x y)))

(def (.list p . ps)
  (.let* ((x p) (xs (if (null? ps) (return ps) (apply .list ps))))
    (cons x xs)))

(def (.make-list count (fill (item)))
  (if (zero? count) (return [])
      (.let* ((x fill) (xs (.make-list (1- count) fill))) [x . xs])))

(def (.list->string p) ((liftP list->string) p))

(def (.string p . ps) (.list->string (apply .list p ps)))

(def (.make-string count (fill (item))) (.list->string (.make-list count fill)))

(def (.string->number p) ((liftP string->number) p))



(def (.not p)
  (.let* (?? #t)
   (.or (.let* (_ p) (set! ?? #f) (fail))
        (.let* (_ (return #!void)) (if ?? (return #t) (fail))))))

(def (.read-line eof-fail?: (eof-fail? #f)
                 include-newline?: (nl? #t)
                 return: (ret list->string))
  (let line ((cs []))
    (.let* (c (.or (item) (return #!eof)))
      (cond ((eof-object? c) (if eof-fail? (fail) (ret (reverse! cs))))
            ((char=? #\newline c) (ret (reverse! (if nl? (cons c cs) cs))))
            (#t (line (cons c cs)))))))

(def (.string=? str (return-parsed? #f) (char? char=?))
  (def (str= lst)
    (if (null? lst)
      (return [])
      (.let* ((c (sat (cut char? <> (car lst))))
              (cs (str= (cdr lst))))
       (if return-parsed? [c . cs] []))))
  (let (lst (string->list str))
    (.let* (v (str= lst)) (if return-parsed? (list->string v) (return str)))))

(def (.string-ci=? str (p? #f))
  (.string=? str p? char-ci=?))
