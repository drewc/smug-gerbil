(import :std/sugar :std/generic :std/ref :std/srfi/1 :std/lazy)
;; return v = \inp -> [(v,inp)]
(def (return v) (lambda (inp) [[v . inp]]))

;; fail = \inp -> []
(def FAIL (lambda _ []))
(def (fail) FAIL)

(defstruct String (point thing) transparent: #t)

(defgeneric input-item-ref (lambda (t n) (ref t n)))
(defgeneric input-item (cut error "No input-item declared for : " <>))
(defmethod (input-item (str <string>)) (input-item (String 0 str)))
(defmethod (input-item (str <pair>)) (input-item (String 0 str)))
(defmethod (input-item (str <vector>)) (input-item (String 0 str)))
(defmethod (input-item (str String))
  (match str
    ((String point parsee)
     (try  [(cons (input-item-ref parsee point)
                  (String (1+ point) parsee))]
           (catch (e) #;(display-exception e) [])))))
(def ITEM input-item)
(def (item) ITEM)

(defgeneric ensure-parser
  (lambda (thing inp)
    ((return thing) inp)))

(defsyntax (:P stx)
  (syntax-case stx ()
    ((macro v)
     (let* (v (syntax->datum #'v))
       (datum->syntax #'macro 
         `(:P ,(cond
                ((char? v) char:)
                ((string? v) string:)
                (((? (or boolean? void? null?)) v) return:)
                ((eof-object? v) eof:)
                (#t ensure:))
              ,v))))

    ((macro char: c)
     #'(sat (cut char=? <> c)))
    ((macro return: v) #'(return v))
    ((macro eof: v) #'(lambda (i) (match (ITEM i)
                               ([] [[v . i]])
                               (t []))))
    ((macro ensure: thing)
     #'(let (v thing)
         (cond
          ((procedure? v) v)
          ((char? v) (:P char: v))
          ((string? v) (:P string: v))
          (((? (or boolean? void? null?)) v) (:P return: v))
          ((eof-object? v) (:P eof: v))
          (#t (cut ensure-parser v <>)))))
    ((macro string: str)
     (let* ((v (syntax->datum #'str))
            (lst? (and (string? v) `(quote ,(string->list v))))
            (str (gensym)) (lst (gensym)) (cs (gensym)))
       (datum->syntax #'macro 
         `(let* ((,str ,v) (,lst ,(or lst? `(string->list ,str))))
            (let str? ((,cs ,lst))
              (if (null? ,cs) (return ,str)
                  (bind (:P char: (car ,cs))
                        (lambda _ (str? (cdr ,cs))))))))))))

;; (import :std/lazy)
(def (bind p f)
  (def (sugarPF f) (lambda (v) (let (r (f v)) (if (procedure? r) r (return r)))))
  (def (callPF PF pair)
    (match pair
      ([v . inp] (((sugarPF PF) v) inp))
      (else
       (error pair " is not a pair as expected for a [v . inp] return value"))))
   (lambda (inp)
     (let lp ((r ((:P p) inp)))
       (match r 
         ([] [])                     
         ([pair . rest]
          (if (lazy? pair)
            (lp (append (force pair) rest))
            (let ((PFr (callPF f pair)))
              (if (null? PFr) (lp rest)
                  (append PFr
                          (if (null? rest) rest
                              (list (delay (let (vs (lp rest))
                                             (if (void? vs) [] vs))))))))))))))

(defsyntax (.let* stx)
  (syntax-case stx ()
   ;;; First the hidden bind: to:
    ((macro bind: id to: PV body ...)
     (datum->syntax
         #'macro
       `(bind (:P ,(syntax->datum #'PV))
              (lambda (,(syntax->datum #'id)) ,@(syntax->datum #'(body ...))))))
  ;;; Now the ((v ...) (w ...)) type that recursively expands.
    ((macro ((id value) rest ...) body ...)
     #'(macro bind: id to: value
              (macro (rest ...) body ...)))
    ((macro (id value) body ...) #'(macro ((id value)) body ...))
    ((macro _ body ...)
     #'(begin body ...))))

;; sat p = item ‘bind‘ \x -> if p x then return x else fail
(def (sat predicate (parser (item)))
  (bind parser (lambda (x) (if (predicate x) (return x) (fail)))))

;; p ++ q = \inp -> (p inp ++ q inp)
(def (++ p q) (lambda (inp) (append ((:P p) inp) ((:P q) inp))))

(def (many p) (++ (.let* ((x p) (xs (many p))) [x . xs]) []))
(def (many1 p) (.let* ((x p) (xs (many p))) [x . xs])) 

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
(def (P:string= str pred: (pred char=?) start: (start #f) end: (end #f)
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
     (P:string= str pred: pred start: start end: end return-parsed: r?)))
(def (.string-ci=? str . args) (apply .string= char-ci=? str args))

(def (some p) (++ [] (.let* ((x p) (xs (some p))) (return [x . xs]))))
(def (some1 p) (.let ((x p) (xs (some p))) (return [x . xs])))
