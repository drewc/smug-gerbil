(import :std/sugar :std/srfi/1 :std/lazy)
(export #t)

;; return v = \inp -> [(v,inp)]
(def (return v) (lambda (inp) [[v . inp]]))

;; fail = \inp -> []
(def (fail) (lambda _ []))

(def (ensure-parser p)
  (cond
   ((procedure? p) p)
   ((char? p)
    (bind (item) (lambda (x) (if (char=? x p) (return p) (fail)))))
   ((string? p)
    (let str ((xs (string->list p)))
      (if (null? xs)
        (return p)
        (bind (item) (lambda (x) (if (char=? x (car xs)) (str (cdr xs)) (fail)))))))
   ((or (boolean? p) (null? p)) (return p))))


;; p ‘bind‘ f = \inp -> concat [f v inp’ | (v,inp’) <- p inp]
;; (def (bind p f) (lambda (inp) (append-map (cut match <> ([v . inp*] ((f v) inp*)))
;; ((ensure-parser p) inp))))


(def (bind p f)
  (def (runPF PF pair) (match pair ([v . inp] ((PF v) inp))
                              (else (error pair " is not a return value pair"))))
  (lambda (inp)
    (let lp ((r ((ensure-parser p) inp)))
      (match r 
        ([] [])                     
        ([pair . rest]
         (if (lazy? pair)
           (lp (append (force pair) rest))
           (let ((PFr (runPF f pair)))
             (if (null? PFr) (lp rest)
                 (append PFr
                         (if (null? rest) rest
                             (list (delay (let (vs (lp rest))
                                            (if (void? vs) [] vs))))))))))))))
;;  item = \inp -> case inp of
;;              [] -> []
;;              (x:xs) -> [(x,xs)]

(defstruct narrow (input start end))


(def (item)
  (def (str-ref thing n)
    (string-ref (let lp ((t thing))
                  (cond ((string? t) t)
                        ((pair? t) (lp (cdr t)))
                        ((narrow? t) (lp (narrow-input t)))))
                n))
  (def (str-item thing n)
    (let ((x (str-ref thing n))
          (xs (cons (+ 1 n) thing)))
      [[x . xs]]))
  (lambda (input)
    (let (inp (if (pair? input)
                input
                (cons 0 input)))
      (try
       (match inp
         ([n . thing]
          (if (and (narrow? thing)
                   (or (< n (narrow-start thing))
                       (> n (narrow-end thing))))
            []
            (str-item thing n))))
       (catch _ [])))))

;; (def (item)
;;   (lambda (input)
;;     (let (inp (if (pair? input)
;;                 input
;;                 (cons 0 input)))
;;       ;(match ([n . thing] inp 
;;       (try
;;        (let ((x (string-ref (cdr inp) (car inp)))
;;              (xs (cons (+ 1 (car inp)) (cdr inp))))
;;          [[x . xs]])
;;        (catch _ [])))))

;; emacs buffer like
(def (point) (lambda (inp) [[(if (pair? inp) (car inp) 0) . inp]])) 

(def (goto-char n) (lambda (inp) [(cons n (cons n (if (pair? inp) (cdr inp) inp)))]))

(def (narrow-to-region start end)
  (lambda (inp) [(cons start (cons start (make-narrow inp start end)))]))

(def (widen)
  (lambda (inp)
    (if (and (pair? inp) (narrow? (cdr inp)))
      (let (nimp (narrow-input (cdr inp)))
        [(cons (narrow-end (cdr inp)) nimp) ])
      [(cons #f inp)])))


(def (run p inp (or-return #f))
  (let lp ((v ((ensure-parser p) inp)))
    (cond ((null? v) or-return)
          ((lazy? (car v))
           (let (new (force (car v)))
             (if (null? new) (lp (cdr v))
                 (lp new))))
          (#t (caar v)))))

;; p ++ q = \inp -> (p inp ++ q inp)

(def (++ p q) (lambda (inp) (append ((ensure-parser p) inp) ((ensure-parser q) inp))))

;; first p = \inp -> case p inp of
;;                    [] -> []
;;                    (x:xs) -> [x]

(def (.first p)
  (lambda (inp) (let (v (p inp)) (match v ([] []) ([x . xs] [x])))))


;; p +++ q = first (p ++ q)
;; We are not lazy, so have to specify.
(def (+++ p q) (lambda (inp) (match ((ensure-parser p) inp)
                          ([] ((ensure-parser q) inp)) (xs xs))))

(def (lazy+ p q)
  (lambda (inp) (match ((ensure-parser p) inp)
             ([] ((ensure-parser q) inp))
             (xs (append xs (list (delay ((ensure-parser q) inp))))))))

(defsyntax (:parser stx)
  (syntax-case stx ()
    ((macro v)
     (let* ((v (syntax->datum #'v))
            (form
             (cond
              ((char? v)
               `(bind (item) (lambda (x) (if (char=? x ,v) (return ,v) (fail)))))
              ((string? v)
               (let (lst (string->list v))
                 `(let str ((xs ',lst))
                   (if (null? xs)
                     (return ,v)
                     (bind (item) (lambda (x)
                                    (if (char=? x (car xs)) (str (cdr xs))
                                        (fail))))))))
              (#t `(ensure-parser ,v)))))
       (with-syntax ((P (datum->syntax #'macro form)))
         #'P)))))

(defsyntax (.let* stx)
  (def (bind-form id value body)
    `(bind ,value (lambda (,id) ,@body)))

  (syntax-case stx ()
    ((macro bind: (values . vs) to: v body ...)
     (let* ((id (gensym)) (MV [':parser (syntax->datum #'v)])
            (MF `(lambda (,id) (let ((values . ,(syntax->datum #'vs)) ,id)
                            ,@(syntax->datum #'(body ...))))))
       (with-syntax ((bf (datum->syntax #'macro ['bind MV MF])))
         #'bf)))
    ((macro bind: id to: value body ...)
     (with-syntax ((bind-form (datum->syntax
                                  #'macro
                                `(bind (:parser ,(syntax->datum #'value))
                                        (lambda (,(syntax->datum #'id))
                                          ,@(syntax->datum #'(body ...)))))))
       #'bind-form))
    ((macro ((id value) rest ...) body ...)
     #'(macro bind: id to: value
              (macro (rest ...) body ...)))
    ((macro (id value) body ...) #'(macro ((id value)) body ...))
    ((macro _ body ...)
     #'(let (ret (begin body ...))
         (if (procedure? ret) ret (return ret))))))
