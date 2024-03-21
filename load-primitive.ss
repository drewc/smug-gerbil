(import :std/sugar :std/generic :std/ref :std/srfi/1 :std/lazy)
;; return v = \inp -> [(v,inp)]
(def (return v) (lambda (inp) [[v . inp]]))

;; fail = \inp -> []
(def FAIL (lambda _ []))
(def (fail) FAIL)

(defstruct String (point thing) transparent: #t)

(defgeneric input-item-ref (lambda (t n) (ref t n)))
(defgeneric input-item (cut error "No input-item declared for : " <>))
(defmethod (input-item (str :string)) (input-item (String 0 str)))
(defmethod (input-item (str :pair)) (input-item (String 0 str)))
(defmethod (input-item (str :vector)) (input-item (String 0 str)))
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

;; p ++ q = \inp -> (p inp ++ q inp)
(def (++ p q) (lambda (inp) (append ((:P p) inp) ((:P q) inp))))

(def (lazy+ p q)
  (lambda (inp) (match ((:P p) inp) ([] ((:P q) inp))
                  (xs (append xs (list (delay ((:P q) inp))))))))
(defalias plus lazy+)

(def (.first p) (lambda (inp) (match ((:P p) inp) ([x . xs] [x]) ([] []))))



(defgeneric input-point (lambda _ 0))
(defmethod (input-point (s String)) (String-point s))

(def POINT (lambda (inp) [[(input-point inp). inp]]))
(def (point) POINT)

(defgeneric input-goto-char
  (lambda (input pos) (input-goto-char (String pos input))))

(defmethod (input-goto-char (inp String) (pos :t))
  (match inp ((String p i) [`(pos . ,(String pos i))])))

(def (goto-char n) (cut input-goto-char <> n))
