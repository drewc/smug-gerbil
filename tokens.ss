;;; -*- Gerbil -*-
;;; (C) me at drewc.ca

(import :drewc/smug/primitive :drewc/smug/simple :std/generic)
(export #t)

;; [[file:~/src/smug-gerbil/tokens.org::*Singular%20Token][]]
(defstruct token (start end type value) transparent: #t)
;; ends here
;; [[file:~/src/smug-gerbil/tokens.org::*Singular%20Token][]]
(def (tokenP p type: (t 'Token) constructor: (makeT make-token))
  (.let* ((b (point)) (v p) (e (point))) (return (makeT b e t v))))
;; ends here
;; [[file:~/src/smug-gerbil/tokens.org::*Singular%20Token][]]
(def (token-reader? p (reader token-value))
  (def (make-str v)
    (cond ((string? v) v)
          ((char? v) (string v))
          (#t (with-output-to-string "" (cut display v)))))
  (.let* (val ((liftP reader) ITEM))
    (if (and (not (procedure? p))
             (eqv? (type-of p) (type-of val)))
      (sat (cut equal? p <>) (return val))
      (let ((str (make-str val))
            (P (if (procedure? p) p
                   (.let* (r p) (return r)))))
        (sat identity (return (run P str)))))))
;; ends here
;; [[file:~/src/smug-gerbil/tokens.org::*Singular%20Token][]]
(def (token-start? p) (token-reader? p token-start))
(def (token-end? p) (token-reader? p token-end))
(def (token-value? p) (token-reader? p token-value))
(def (token-type? p) (token-reader? p token-type))
;; ends here
;; [[file:~/src/smug-gerbil/tokens.org::*/syntax/%20~Token~][]]
(defsyntax (Token stx)
  (syntax-case stx ()
    ((macro [args ...] body ...)
     (datum->syntax #'macro `(.or (tokenP ,@(syntax->datum #'(args ...)))
                                  (Token ,@(syntax->datum #'(body ...))))))
    ((macro t rest ...)
     #'(macro [t type: 't] rest ...))
    (_ #'FAIL)))
;; ends here
