;;; Breno Lino Prado - 202265013AC
;;; Gabriel Arantes Resende Pereira - 202065126A

#lang racket

(require dcc019/util/env
         dcc019/util/memory
         dcc019/exercise/iclasses/ast)

(provide value-of-program)

(struct object (class-name fields))
(struct class (super-name field-names method-env))
(struct method (vars body super-name fields))

(define class-env '())

;; --- Cálculo de Valores ---

;; Mapeia valores das expressões
(define (map-value-of exps Δ)
  (map (lambda (exp) (value-of exp Δ)) exps))

;; Avalia a expressão
(define (value-of exp Δ)
  (match exp
    [(ast:int v) v]
    [(ast:bool v) v]
    [(ast:dif e1 e2) (- (value-of e1 Δ) (value-of e2 Δ))]
    [(ast:zero? e) (zero? (value-of e Δ))]
    [(ast:not e) (not (value-of e Δ))]
    [(ast:if e1 e2 e3) (if (value-of e1 Δ) (value-of e2 Δ) (value-of e3 Δ))]
    [(ast:let (ast:var x) e1 e2)
     (let ([val (value-of e1 Δ)]) 
       (let ([new-env (extend-env x (newref val) Δ)])
         (value-of e2 new-env)))]
    [(ast:var v) (deref (apply-env Δ v))]
    [(ast:send e (ast:var mth) args)
     (let* ([args-with-value (map-value-of args Δ)]
            [obj (value-of e Δ)])
       (apply-method (find-method (object-class-name obj) mth) obj args-with-value))]
    [(ast:super (ast:var c) args)
     (let* ([args-with-value (map-value-of args Δ)]
            [obj (apply-env Δ "self")])
       (apply-method (find-method (apply-env Δ "super") (ast:var-name args)) obj args-with-value))]
    [(ast:self) (apply-env Δ "self")]
    [(ast:new (ast:var c) args)
     (let* ([args-with-value (map-value-of args Δ)]
            [obj (let* ([class (find-class c)]
                        [field-names (class-field-names class)]
                        [fields (map (lambda (field-name) (newref null)) field-names)])
                   (object c fields))])
       (apply-method (find-method c "initialize") obj args-with-value)
       obj)]
    [e (raise-user-error "Não implementado: " e)]))

;; --- Execução de Declarações ---

;; Executa a declaração
(define (result-of stmt Δ)
  (match stmt
    [(ast:assign (ast:var x) e)
     (setref! (apply-env Δ x) (value-of e Δ))]
    [(ast:print e)
     (display (value-of e Δ))
     (newline)]
    [(ast:return e) (value-of e Δ)]
    [(ast:block stmts) (for ([s stmts]) (result-of s Δ))]
    [(ast:if-stmt e s1 s2) (if (value-of e Δ) (result-of s1 Δ) (result-of s2 Δ))]
    [(ast:while e s) (when (value-of e Δ)
                       (result-of s Δ)
                       (result-of stmt Δ))]
    [(ast:local-decl (ast:var x) s) (result-of s (extend-env x (newref 'null) Δ))]
    [(ast:send e (ast:var mth) args)
     (let* ([args-with-value (map-value-of args Δ)]
            [obj (value-of e Δ)])
       (apply-method (find-method (object-class-name obj) mth) obj args-with-value))]
    [(ast:super (ast:var c) args)
     (let* ([args-with-value (map-value-of args Δ)]
            [obj (apply-env Δ "self")])
       (apply-method (find-method (apply-env Δ "super") c) obj args-with-value))]
    [e (raise-user-error "Não implementado: " e)]))

;; --- Gerenciamento de Ambiente e Classes ---

;; Adiciona a classe ao ambiente
(define (add-class class-name class-list)
  (unless (class-exists? class-name class-list)
    (set! class-env (cons (cons class-name class-list) class-env))))

;; Mescla declarações de métodos com métodos da superclasse
(define (merge-method m-decls super-name fields)
  (append
   (map (lambda (m-decl) (create-method super-name fields m-decl)) m-decls)
   (class-method-env (find-class super-name))))

;; Cria a estrutura de método
(define (create-method super-name fields m-decl)
  (list (ast:var-name (ast:method-name m-decl))
        (method (map ast:var-name (ast:method-params m-decl))
                (ast:method-body m-decl)
                super-name fields)))

;; Encontra a classe pelo nome
(define (find-class class-name)
  (let ([class-pair (assoc class-name class-env)])
    (if class-pair
        (cdr class-pair)
        (raise-user-error "Classe não encontrada: " class-name))))

;; Verifica se a classe existe
(define (class-exists? class-name class-list)
  (let ([existing-class (find-class-exists class-name)])
    (and existing-class
         (equal? (class-field-names existing-class) (class-field-names class-list))
         (equal? (class-method-env existing-class) (class-method-env class-list)))))

;; Encontra a classe existente
(define (find-class-exists class-name)
  (let ([maybe-pair (assoc class-name class-env)])
    (and maybe-pair (cdr maybe-pair))))

;; Adiciona nomes de campos para incluir campos da superclasse
(define (append-field-names super-fields self-fields)
  (foldr (lambda (field acc)
           (if (member field acc)
               (append acc (list (string-append field "%1")))
               (append acc (list field))))
         self-fields
         super-fields))

;; --- Manipulação de Métodos ---

;; Aplica um método a um objeto com argumentos dados
(define (apply-method method self args)
  (let* ([args-with-refs (map newref args)]
         [extended-env (extend-env "self" self
                                   (extend-env "super" (method-super-name method) empty-env))]
         [method-env (bind-vars (method-fields method) (object-fields self) extended-env)])
    (result-of (method-body method)
               (bind-vars (method-vars method) args-with-refs method-env))))

;; Associa variáveis aos seus valores no ambiente
(define (bind-vars vars values env)
  (for ([var vars] [val values])
    (set! env (extend-env var val env)))
  env)

;; Encontra um método pelo nome da classe e do método
(define (find-method class-name method-name)
  (let ([m-env (class-method-env (find-class class-name))])
    (let ([maybe-pair (assoc method-name m-env)])
      (if maybe-pair
          (cadr maybe-pair)
          (raise-user-error "Método não encontrado: " method-name)))))

;; --- Avaliação do Programa ---

;; Avalia o programa
(define (value-of-program prog)
  (empty-store)
  (match prog
    [(ast:prog decls stmt)
     (begin
       (add-class "object" (class #f '() '()))
       (for ([decl decls])
         (let* ([class-name (ast:var-name (ast:decl-name decl))]
                [super-name (ast:var-name (ast:decl-super decl))]
                [super-fields (class-field-names (find-class super-name))]
                [fields (append-field-names super-fields (get-field-names (ast:decl-fields decl)))]
                [methods (merge-method (ast:decl-methods decl) super-name fields)]
                [class (class super-name fields methods)])
           (add-class class-name class)))
       (result-of stmt init-env))]))
