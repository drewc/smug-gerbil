;;; -*- Gerbil -*-
;;; (C) me at drewc.ca
(import :drewc/smug/primitive :std/srfi/1) 
(export #t)


;; [[file:~/src/smug-gerbil/parser.org::*~liftP~,%20lift%20a%20function%20to%20a%20parser][]]
(def (liftP fn) (cut bind <> (lambda (r) (return (fn r)))))
;; ends here
;; [[file:~/src/smug-gerbil/parser.org::*~skip~,%20skip%20to%20me%20lex%20my%20darling.][]]
(def (skip p) (.or (.let* (_ p) (.or (skip p) #t)) #f))
;; ends here

;; IMPORTED FROM PRIMITIVE ;; [[file:~/src/smug-gerbil/parser.org::*~sat~,%20our%20first%20simple%20parser.][]]
;; IMPORTED FROM PRIMITIVE ;; sat p = item ‘bind‘ \x -> if p x then return x else fail
;; IMPORTED FROM PRIMITIVE (def (sat predicate (parser (item)))
;; IMPORTED FROM PRIMITIVE   (bind parser (lambda (x) (if (predicate x) (return x) (fail)))))
;; IMPORTED FROM PRIMITIVE ;; ends here

;; [[file:~/src/smug-gerbil/parser.org::*~lazy+~,%20~many~%20and%20~some~%20put%20to%20rest%20as%20we're%20not%20lazy%20enough.][]]
(def (many p) (lazy+ (.let* ((x p) (xs (many p))) [x . xs]) []))
(def (some p) (lazy+ [] (.let* ((x p) (xs (some p))) [x . xs])))
;; ends here
;; [[file:~/src/smug-gerbil/parser.org::*~some~%20and%20~some1~,%20Part%201][]]
(def (some1 p) (.let* ((x p) (xs (some p))) (return [x . xs])))
;; ends here
;; [[file:~/src/smug-gerbil/parser.org::*~many1~,%20because%20empty%20is%20boring][]]
(def (many1 p) (.let* ((x p) (xs (many p))) [x . xs])) 
;; ends here

;; [[file:~/src/smug-gerbil/parser.org::*~plus~%20and%20~.any~.][]]
(def (.any . ps) (foldr plus FAIL ps))
;; ends here

;; [[file:~/src/smug-gerbil/parser.org::*~.or~%20and%20~+++~][]]
(def (.or . ps) (.first (apply .any ps)))
(def (+++ p q) (.first (plus p q)))
;; ends here

;; [[file:~/src/smug-gerbil/parser.org::*~sepby1~][]]
(def (sepby1 p sep)
  (.let* ((x p) (xs (many (.let* ((_ sep) (y p)) y)))) [x . xs]))

;; ((sepby1 Int ",") "-42,42,420")
;; => (((-42 42 420) . #<String point: 10 thing: "-42,42,420">)
;;     ((-42 42 42) . #<String point: 9 thing: "-42,42,420">)
;;     ((-42 42 4) . #<String point: 8 thing: "-42,42,420">)
;;     ((-42 42) . #<String point: 6 thing: "-42,42,420">)
;;     ((-42 4) . #<String point: 5 thing: "-42,42,420">)
;;     ((-42) . #<String point: 3 thing: "-42,42,420">)
;;     ((-4) . #<String  point: 2 thing: "-42,42,420">))
;; ends here
;; [[file:~/src/smug-gerbil/parser.org::*~bracket~][]]
(def (bracket open p close) (.let* ((_ open) (x p) (_ close)) x))
;; ends here
;; [[file:~/src/smug-gerbil/parser.org::*~sepby~,%20like%20~many~,%20always%20succeeds.][]]
(def (sepby p sep) (++ (sepby1 p sep) []))
;; ends here

;; [[file:~/src/smug-gerbil/parser.org::*~characters~'s][]]
(defsyntax (def.charsat stx)
  (syntax-case stx ()
    ((m pred)
     (let (pred? (syntax->datum #'pred))
     (datum->syntax #'m
       `(def (,(string->symbol
                (string-append "." (symbol->string pred?))) c)
          (sat (cut ,pred? <> c))))))
    ((m pred preds ...)
     #'(begin (m pred)
              (m preds ...)))))

(def.charsat char=? char>? char<? char<=? char>=?
  char-ci=? char-ci<=? char-ci>=? char-ci<? char-ci>?)

(def (.char-alphabetic?) (sat char-alphabetic?))
(def (.char-numeric?) (sat char-numeric?))
(def (.char-upper-case?) (sat char-upper-case?))
(def (.char-lower-case?) (sat char-lower-case?))
;; ends here
;; [[file:~/src/smug-gerbil/parser.org::*~.list=~][]]
(def* .list=
  ((lst) (.list= equal? lst #t))
  ((pred-or-list list-or-bool)
   (.list= (if (list? pred-or-list) equal? pred-or-list)
           (if (list? pred-or-list) pred-or-list list-or-bool)
           (if (list? pred-or-list) list-or-bool #t)))
  ((elt= lst return-parsed?)
   (let l= ((cs lst))
     (if (null? cs) (return [])
         (.let* ((c (sat (cut elt= <> (car cs))))
                 (cs (l= (cdr cs))))
           (if return-parsed? (cons c cs) lst))))))
;; ends here
;; [[file:~/src/smug-gerbil/parser.org::*~.string%5B-ci%5D=%5B?%5D~,%20~:P~%20is%20not%20the%20only%20way%20to%20test%20strings.][]]
(def (.string=? str (start #f) (end #f))
  (:P string:
      (if (not (or start end)) str
          (substring
           str (or start 0) (or end (string-length str))))))
;; ends here
;; [[file:~/src/smug-gerbil/parser.org::*~.string%5B-ci%5D=%5B?%5D~,%20~:P~%20is%20not%20the%20only%20way%20to%20test%20strings.][]]
(def (:Pstring= str pred: (pred char=?) start: (start #f) end: (end #f)
                return-parsed: (r? #t))
  (def lst (string->list
            (if (or (not (or start end))
                    (and (eqv? start 0) (not end)))
             str
             (substring str (or start 0) (or (and (number? end) end) (string-length str))))))

  (.let* (l (.list= pred lst r?))
    (if r? (list->string l) str)))
;; ends here
;; [[file:~/src/smug-gerbil/parser.org::*~.string%5B-ci%5D=%5B?%5D~,%20~:P~%20is%20not%20the%20only%20way%20to%20test%20strings.][]]
(def (.string= pred-or-str (str-or-n-or-b (void))
               (n-or-b (void))
               (en-or-b (void))
               (r? (void)))
 (let ((str (if (string? pred-or-str) pred-or-str str-or-n-or-b))
       (pred (if (string? pred-or-str) char=? pred-or-str))
       (start (if (number? str-or-n-or-b) str-or-n-or-b
                  (if (number? n-or-b) n-or-b (if (number? en-or-b) en-or-b #f))))
       (end (if (string? pred-or-str)
              (if (number? str-or-n-or-b)
                n-or-b
                (if (number? en-or-b) en-or-b #f))
              #f))
       (r? (if (boolean? r?) r?
               (if (boolean? en-or-b) en-or-b
                   (if (boolean? n-or-b) n-or-b
                       (if (boolean? str-or-n-or-b) str-or-n-or-b #t))))))
     (:Pstring= str pred: pred start: start end: end return-parsed: r?)))
;; ends here
;; [[file:~/src/smug-gerbil/parser.org::*~.string-ci=?~%20and%20friends.][]]
(def (.string-ci=? str . args) (apply .string= char-ci=? str args))
;; ends here

;; [[file:~/src/smug-gerbil/parser.org::*Repetition%20with%20meaningful%20separators][]]
(def (ops . pairs)
  (def op (cut match <> ([p . op] (.let* (_ p) (return op)))))
  (foldr ++ FAIL (map op pairs)))
;; ends here
;; [[file:~/src/smug-gerbil/parser.org::*Repetition%20with%20meaningful%20separators][]]
(def (chainl1 p op)
  (def (chain-link x)
    (++ (.let* ((f op) (y p)) (chain-link (f x y))) (return x)))
  (bind p chain-link))
;; ends here
;; [[file:~/src/smug-gerbil/parser.org::*Repetition%20with%20meaningful%20separators][]]
(def (chainr1 p op)
  (.let* (x p)
    (++ (.let* ((f op) (y (chainr1 p op))) (return (f x y)))
        (return x))))
;; ends here

;; [[file:~/src/smug-gerbil/parser.org::*~.begin~%20and%20~.begin0~][]]
(defsyntax (.begin stx)
  (def ret (gensym))
  (syntax-case stx ()
    ((m bind: P Ps ...)
     (let* ((restps (syntax->datum #'(Ps ...)))
            (rest-form (if (null? restps) `(return ,ret)
                        `(.begin ,@restps)))
            (p (syntax->datum #'P)))
       (datum->syntax #'m
         `(.let* (,(if (null? restps) ret '_) ,p) ,rest-form))))
    ((m P Ps ...)
     #'(lambda (inp) ((m bind: P Ps ...) inp)))))

;; (defrules .begin ()
;;     ((macro p)
;;      (.let* (x p) (return x)))
;;     ((macro p ps ...)
;;      (.let* (_ p) (macro ps ...))))

;; (defsyntax (.begin stx)
;;   (def inp (gensym))
;;   (def ret (gensym))
;;   (syntax-case stx ()
;;     ((macro P)
;;      (datum->syntax
;;          #'macro `(lambda (,inp) ((.let* (,ret ,(syntax->datum #'P))
;;                                (return ,ret))))
;;     ((macro P Ps ...)
;;      (datum->syntax #'macro `(.let* (_ ,(syntax->datum #'P))
;;                                          (.begin ,@(syntax->datum #'(Ps ...))))))))
(defrules .begin0 ()
  ((macro p)
   (.begin p))
  ((macro p ps ...)
   (.begin (.let* ((x p)
                   (_ (.begin ps ...)))
             (return x)))))


;; ends here

;; [[file:~/src/smug-gerbil/parser.org::*~.cons%5B*%5D~,%20~.%5Bmake-%5D%5Blist|string%5D~,%20~.%5Blist|string%5D->%5Bstring|number%5D~][]]
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
;; ends here

;; [[file:~/src/smug-gerbil/parser.org::*~ci=?~.%20Case%20insensitivity%20arises%20a%20lot.][]]
(def (ci=? t)
  (cond ((char? t) (.char-ci=? t))
        ((string? t) (.string-ci=? t))
        ((list? t) (.list= char-ci=? t))))
;; ends here

;; [[file:~/src/smug-gerbil/parser.org::*~.not~%20and%20~peek~][]]
(def (peek p)
  (def npek (gensym))
  (def (peeked? v) (not (eq? v npek)))
  (.let* ((peek? npek)
          (_ (.or (.let* (r p) (set! peek? r) FAIL) (void))))
    (sat peeked? (return peek?))))
;; ends here

;; [[file:~/src/smug-gerbil/parser.org::*~.not~%20and%20~peek~][]]
(def (.not p)
  (let (no (gensym)) (sat (cut eq? <> no) (.or p (return no)))))
;; ends here


;;; TODO: Replace soon -- me@drewc.ca
(def (.read-line eof-fail?: (eof-fail? #f)
                 include-newline?: (nl? #t)
                 return: (ret list->string))
  (let line ((cs []))
    (.let* (c (.or (item) (return #!eof)))
      (cond ((eof-object? c) (if eof-fail? (fail) (ret (reverse! cs))))
            ((char=? #\newline c) (ret (reverse! (if nl? (cons c cs) cs))))
            (#t (line (cons c cs)))))))
