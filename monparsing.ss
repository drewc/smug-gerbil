(import :std/iter :std/srfi/13 (for-syntax :drewc/smug/interface))

(def (result v) (lambda (inp) [[v . inp]]))

(def (zero) (lambda _ []))

(def (item)
  (lambda (inp) 
    (if (string-null? inp)
      []
      [(cons (string-ref inp 0) (string-drop inp 1))])))


(def (bind p f) (lambda (inp) (apply append (for/collect  ([v . inp*] (p inp)) ((f v) inp*)))))

(def (seq p q)
  (bind p (lambda (x) (bind q (lambda (y) (result (cons x y)))))))

(def (sat p)
  (bind (item) (lambda (x) (if (p x) (result x) (zero)))))

(def (char x) (sat (cut char=? x <>)))

(def (digit) (sat (cut char-numeric? <>)))

(def (lower) (sat (cut char-lower-case? <>)))

(def (upper) (sat (cut char-upper-case? <>)))

(def (plus p q)
  (lambda (inp) (append (p inp) (q inp))))

(def (letter) (plus (lower) (upper)))
(def (alphanum) (plus (letter) (digit)))

(def (word)
  (def (neWord)
    (bind (letter) (lambda (x) (bind (word) (lambda (xs) (result (format "~a~a" x xs)))))))
  (plus (neWord) (result "")))


(begin-syntax 
 (define-interface-class Monad
   (result bind))
 (define-interface-class (Monad0Plus Monad)
   (zero ++)))

(def Parser
  (make-interface 'Monad0Plus
     ;; result :: a -> Parser a
     result: (lambda (v) (lambda (inp) [[v . inp]]))
     ;; bind :: Parser a -> (a -> Parser b) -> Parser
     bind: (lambda (p f) (lambda (inp) (apply append (for/collect  ([v . inp*] (p inp)) ((f v) inp*)))))
     ;; zero :: Parser a
     zero: (lambda () (lambda _ []))
     ;; (++) :: Parser a -> Parser a -> Parser a
     ++: (lambda (p q) (lambda (inp) (append (p inp) (q inp))))))

(defsyntax (mlet* stx)
  (def (bind-form id value body)
    `(bind ,value (lambda (,id) ,@body)))
  (syntax-case stx ()
    ((macro bind: id to: value body ...)
     (with-syntax ((bind-form (datum->syntax
                          #'macro (bind-form (syntax->datum #'id)
                                             (syntax->datum #'value)
                                             (syntax->datum #'(body ...))))))
       #'bind-form))
    ((macro ((id value) rest ...) body ...)
     #'(macro bind: id to: value
            (macro (rest ...) body ...)))
    ((macro () body ...)
     #'(begin body ...))))

(def current-parser (make-parameter Parser))

(defsyntax (defp stx)
  (syntax-case stx ()
    ((macro (parser args ...) body ...)
     (let ((b (syntax->datum #'(body ...))))
       (with-syntax ((l (datum->syntax #'macro
                          `(with-interface (Monad0Plus (current-parser)) ((lambda _ ,@b))))))
         #'(def (parser args ...) l))))))

(defp (string str)
   (if (string-null? str)
       (result "")
       (mlet* ((_ (char (string-ref str 0)))
               (_ (string (string-drop str 1))))
        (result str))))

(def (many p)
  (with-interface Parser (++ (mlet* ((x p) (xs (many p))) (result [x . xs])) (result []))))

(defp (many1 p) (mlet* ((x p) (xs (many p))) (result [x . xs])))

(defp (nat)
  (def (evals xs)
    (result (with-input-from-string (list->string xs) read)))
  (mlet* ((xs (many1 (digit)))) (result xs)))

(defp (int) 
  (def (op) (++ (let*-monad ((_ (char #\-))) (result (cut - <>))) (result identity)))
    (let*-monad ((f (op)) (n (nat))) (result (f n))))
