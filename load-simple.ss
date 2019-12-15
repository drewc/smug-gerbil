;; sat p = item ‘bind‘ \x -> if p x then return x else fail
(def (sat predicate (parser (item)))
  (bind parser (lambda (x) (if (predicate x) (return x) (fail)))))

(def (liftP fn) (cut bind <> (lambda (r) (return (fn r)))))

(def (many p) (lazy+ (.let* ((x p) (xs (many p))) [x . xs]) []))
(def (some p) (lazy+ [] (.let* ((x p) (xs (some p))) [x . xs])))
(def (some1 p) (.let ((x p) (xs (some p))) (return [x . xs])))
(def (many1 p) (.let* ((x p) (xs (many p))) [x . xs])) 

(def (.any . ps) (foldr plus FAIL ps))

(def (.or . ps) (.first (apply .any ps)))
(def (+++ p q) (.first (plus p q)))

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
(def (bracket open p close) (.let* ((_ open) (x p) (_ close)) x))
(def (sepby p sep) (++ (sepby1 p sep) []))

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
(def (.string=? str (start #f) (end #f))
  (:P string:
      (if (not (or start end)) str
          (substring
           str (or start 0) (or end (string-length str))))))
(def (:Pstring= str pred: (pred char=?) start: (start #f) end: (end #f)
                return-parsed: (r? #t))
  (def lst (string->list
            (if (or (not (or start end))
                    (and (eqv? start 0) (not end)))
             str
             (substring str (or start 0) (or (and (number? end) end) (string-length str))))))

  (.let* (l (.list= pred lst r?))
    (if r? (list->string l) str)))
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
(def (.string-ci=? str . args) (apply .string= char-ci=? str args))

(def (ops . pairs)
  (def op (cut match <> ([p . op] (.let* (_ p) (return op)))))
  (foldr ++ FAIL (map op pairs)))
(def (chainl1 p op)
  (def (chain-link x)
    (++ (.let* ((f op) (y p)) (chain-link (f x y))) (return x)))
  (bind p chain-link))
(def (chainr1 p op)
  (.let* (x p)
    (++ (.let* ((f op) (y (chainr1 p op))) (return (f x y)))
        (return x))))

(defrules .begin ()
    ((macro p)
     (.let* (x p) (return x)))
    ((macro p ps ...)
     (.let* (_ p) (macro ps ...))))

(defrules .begin0 ()
  ((macro p)
   (.let* (x p) (return x)))
  ((macro p ps ...)
   (.let* ((x p) (_ (.begin (macro ps ...)))) (return x))))
