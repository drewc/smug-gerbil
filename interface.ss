(module istructs-and-classes
  (export #t)
  (defstruct interface-symbol (name))
  
  (defstruct (interface-slot interface-symbol)
    (value))
  
  (defstruct (interface-inline interface-symbol)
    (form))
  
  (defstruct (interface-alias interface-symbol)
    (to-name))
  (def interface-symbol-table (make-hash-table-eq))
  (def (interface-symbol-put! klass interface-symbol)
    (let* ((syms (hash-ref interface-symbol-table klass))
           (existing (member interface-symbol syms
                             (lambda args (apply eq? (map interface-symbol-name args))))))
      (if existing
        (set-car! existing interface-symbol)
        (hash-put! interface-symbol-table klass (cons* interface-symbol syms)))))
  (defclass Interface () constructor: :init!)
  
  (defmethod {:init! Interface}
    (cut class-instance-init! <...>))
  
  (defmethod {interface-symbols Interface}
    (lambda _ [])))

(import :gerbil/core 
 (for-syntax :std/format)
 :std/format 
 (for-syntax :std/srfi/1)
 :std/srfi/1
 (for-syntax istructs-and-classes)
 istructs-and-classes
 :std/iter :std/sugar :std/generic 
 (for-syntax :std/misc/rtd)) 
(export with-interface define-interface-class
        make-interface make-interface-class
        interface-symbol interface-symbol-name
        interface-symbol-table interface-symbol-put!
        interface-slot interface-slot-value
        make-interface-inline interface-inline interface-inline-form
        define-interface-inline)


(def (description->interface-symbol description)
  (cond
   ;; If the description is just a symbol, this a reference to a slot.
   ;; Use the absent-obj so we do not have a default value.
   ((symbol? description)
    (make-interface-slot description absent-obj))
   ;; If it's a list, match it!
   ((list? description)
    (match description
      ;; (name inline: value) is inline form
      ([name inline: form] (make-interface-inline name form))
      ;; ((name . args) . body) is an inline form with (lambda args body ...)
      ([[name . args] . body] (make-interface-inline name `(lambda ,args ,@body)))
      ;; (name alias: to-name) is an alias
      ([name alias: to-name] (make-interface-alias name to-name))
      ;; otherwise, slot and default value
      ([name default] (make-interface-slot name default))))
   (else (error "Invalid Interface Description syntax"))))

(def (bind-interface-init! klass interface-symbols)
  (bind-method! 
   klass ':init! 
   (lambda (self . args)
     (def (add-args (syms interface-symbols))
       (let* ((isym (car syms))
              (sym (interface-symbol-name isym))
              (key (symbol->keyword sym)))
         (when (and (interface-slot? isym)
                  (not (eqv? absent-obj (interface-slot-value isym)))
                  (not (member key args)))
           (set! args (append [key (interface-slot-value isym)] args))) 
           (unless (null? (cdr syms)) (add-args (cdr syms)))))
     (add-args)
     (apply call-next-method klass self ':init! args))))

(def (make-interface-class name supers descriptions)
 (def interface-symbols (map description->interface-symbol descriptions))
 (def interface-slots (filter interface-slot? interface-symbols))

 (def interface-supers (map (lambda (s)
                              (if (class-type? s) s (object-type s)))
                            supers))
 (def (create-interface-class)
   (def slots (map interface-symbol-name interface-slots))
   (make-class-type
    name (append interface-supers [Interface::t])
    slots (string->symbol (format "Interface: ~A" name))
    [] ':init!))

 (def (filter-symbols interface-symbols syms)
   ;; (displayln "Filtering")
   (let (names (map interface-symbol-name interface-symbols))
     ;;  (displayln "Got " names " for " syms "using" remove )
     (remove (lambda (s)
               ;;           (displayln "remove? " (interface-symbol-name s)
               ;;                     (member (interface-symbol-name s) names))
               (member (interface-symbol-name s) names))
             syms)))

 (let ((klass (create-interface-class)))
   (begin0 klass
     ;; Bind the symbols(
     (hash-put! interface-symbol-table klass interface-symbols)
     (bind-method! klass 'interface-symbols
                   (lambda (obj) 
                     (let (is (hash-ref interface-symbol-table klass))
                              (append is 
                                      (filter-symbols
                                       is (call-next-method klass obj 'interface-symbols))))))
     ;; Bind :init!
     (bind-interface-init! klass interface-symbols))))

(defsyntax (define-interface-class stx)
 (def (description-form->description form)
   (match form
     ([[name . args] . forms] ['list ['quote name]
                                     inline: `(quote (lambda ,args ,@forms))])
     ([name value] ['list ['quote name] value])
     ([alias alias: name] ['list ['quote alias] alias: ['quote name]])
     ([name inline: . forms] ['list ['quote name]
                                    inline: (cons* 'quote (if (null? (cdr forms))
                                                            forms
                                                            (cons 'begin forms)))])
     ([name keyword . args] ['list ['quote name] keyword . args])
     (symbol ['quote symbol])))
 (def (singleton? args)
   (let* (name (member instance: args))
        (if name (cadr name) #f))) 

  (syntax-case stx ()
    ((macro (interface supers ...) descriptions args ...)
     (let (instance (singleton? (syntax->datum #'(args ...))))
     (with-syntax ((ds (datum->syntax #'macro (cons 'list (map description-form->description
                                        (syntax->datum  #'descriptions)))))
                   (name (datum->syntax #'macro (string->symbol
                                                 (string-append (symbol->string (syntax->datum #'interface))
                                                                "::interface"))))
                   (super-interfaces (datum->syntax
                                         #'macro (cons 'list (map (lambda (s)
                                                                    (string->symbol
                                                                     (string-append (symbol->string s) "::interface")))
                                                                  (syntax->datum #'(supers ...))))))
                   (defi (datum->syntax #'macro (when instance
                                                  `(def ,(if (eq? instance #t)
                                                           (syntax->datum #'interface)
                                                           instance)
                                                     (make-interface ',(syntax->datum #'interface)))))))


       #'(begin (define name (make-interface-class 'interface super-interfaces ds))
                defi
                'name))))
    ((macro class descriptions args ...)
     #'(macro (class) descriptions args ...))))

(def (find-interface-class name)
  (eval (string->symbol (string-append (symbol->string name)
                                   "::interface"))))

(def (make-interface interface . args)
  (apply make-class-instance
    (cond
     ((class-type? interface) interface)
     ((symbol? interface)
           (eval (string->symbol (string-append (symbol->string interface)
                                 "::interface"))))
     (else (object-type interface)))
    args))

(defsyntax (: interface)
  (syntax-case interface ()
    ((macro name args ...)
     (with-syntax ((name (datum->syntax #'macro (string->symbol
                                                 (string-append (symbol->string (syntax->datum #'name))
                                                                  "::interface")))))
       #'(make-interface name args ...)))))

(defsyntax (define-interface-inline stx)
  (syntax-case stx ()
    ((macro interface-class-name (name args ...) body ...)
     (let (inline-form `(lambda ,(syntax->datum #'(args ...))
                           ,@(syntax->datum #'(body ...))))
     (with-syntax ((iname (datum->syntax #'macro (syntax->datum #'name)))
                   (iform (datum->syntax #'macro inline-form)))
       #'(interface-symbol-put! (find-interface-class 'interface-class-name)
                                (make-interface-inline 'iname 'iform)))))


    ((macro class-name name body ...)
     #'(interface-symbol-put! (find-interface-class 'class-name)
                              (make-interface-inline 'name '(begin body ...))))))

(begin-syntax 
  (defmethod {interface-symbol-form interface-symbol}
    (lambda (self interface-binding-name) #!void))
  
  (defmethod {interface-symbol-form interface-slot}
    (lambda (self name)
      `(unchecked-slot-ref ,name ',(interface-symbol-name self))))
  
  (defmethod {interface-symbol-form interface-inline}
    (lambda (self _)
      (interface-inline-form self)))
  
  (defmethod {interface-symbol-form interface-alias}
    (lambda (self _)
       (interface-alias-to-name self))))

(defsyntax (with-interface stx)
  (def (interface-symbol->letrec*-binding interface-symbol interface-instance interface-binding)
    [(interface-symbol-name interface-symbol)
     {interface-symbol-form interface-symbol interface-binding}])

  (syntax-case stx (:)
    ((macro (class: form interface: interface) body ...)
     (let* ((interface-binding (gensym))
            (interface-instance (eval `(: ,(syntax->datum #'form))))
            (bindings (map (cut interface-symbol->letrec*-binding
                             <> interface-instance interface-binding)
                        {interface-symbols interface-instance})))

       (with-syntax ((rec-bindings (datum->syntax #'macro bindings))
                     (interface-let-name (datum->syntax #'macro interface-binding)))
         #'(let (interface-let-name interface) (letrec* rec-bindings body ...)))))
;;; If we are using the (: class ...) macro, use (class (: class ...)) as
;;; the interface
    ((macro (: name args ...) body ...)
     #'(macro (class: name interface: (: name args ...)) body ...))
;;; Now (class/interface interface) 
    ((macro (expr interface) body ...)
     (let* ((intername (gensym))
            (interform (syntax->datum #'expr))
            ;; Are we given an interface instance or a class
            (instance (with-catch (lambda (_) #f) (lambda () (eval interform)))))
       (with-syntax ((class-name (type-id (object-type (or instance (eval `(: ,interform)))))))
         #'(macro (class: class-name interface: interface) body ...))))
    ((macro (interface rest ...) body ...)
     #'(macro (interface (: interface rest ...))
         body ...))
    ((macro expr body ...)
     #'(macro (expr expr) body ...))))
