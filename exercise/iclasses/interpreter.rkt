; Breno Lino Prado - 202265013AC
; Gabriel Arantes Resende Pereira - 202065126A

#lang racket

(require dcc019/util/env
         dcc019/util/memory
         dcc019/exercise/iclasses/ast)

(provide value-of-program)

; Estruturas principais da linguagem ICLASSES:
; - object: Objeto com nome da classe e campos.
; - class: Classe com superclasse, campos e métodos.
; - method: Método com variáveis, corpo, superclasse e campos.
(struct object (class-name fields))
(struct class (super-name field-names method-env))
(struct method (vars body super-name fields))

; Ambiente de classes inicializado como lista vazia
(define class-env '())

; --- Cálculo de Valores ---

; Avalia uma lista de expressões no ambiente dado
(define (map-value-of exps Δ)
  (map (lambda (exp) (value-of exp Δ)) exps))

; Avalia uma expressão conforme a semântica da linguagem
(define (value-of exp Δ)
  (match exp
    [(ast:int v) v]
    [(ast:bool v) v]
    [(ast:dif e1 e2) (- (value-of e1 Δ) (value-of e2 Δ))]
    [(ast:zero? e) (zero? (value-of e Δ))]
    [(ast:not e) (not (value-of e Δ))]
    [(ast:if e1 e2 e3) (if (value-of e1 Δ) (value-of e2 Δ) (value-of e3 Δ))]

    ; Avalia let, estendendo o ambiente com uma nova variável
    [(ast:let (ast:var x) e1 e2)
     (let ([val (value-of e1 Δ)]) 
       (value-of e2 (extend-env x (newref val) Δ)))]

    [(ast:var v) (deref (apply-env Δ v))]

    ; Avalia uma chamada de método no objeto e aplica o método
    [(ast:send e (ast:var mth) args)
     (let* ([args-with-value (map-value-of args Δ)]
            [obj (value-of e Δ)])
       (apply-method (find-method (object-class-name obj) mth) obj args-with-value))]

    ; Avalia uma chamada de método na superclasse do objeto
    [(ast:super (ast:var c) args)
     (let* ([args-with-value (map-value-of args Δ)]
            [obj (apply-env Δ "self")])
       (apply-method (find-method (apply-env Δ "super") (ast:var-name args)) obj args-with-value))]

    ; Retorna o próprio objeto (self) no método
    [(ast:self) (apply-env Δ "self")]

    ; Cria um novo objeto da classe e inicializa campos
    [(ast:new (ast:var c) args)
     (let* ([args-with-value (map-value-of args Δ)]
            [obj (let* ([class (find-class c)]
                        [field-names (class-field-names class)]
                        [fields (map (lambda (field-name) (newref null)) field-names)]) (object c fields))])
       (apply-method (find-method c "initialize") obj args-with-value) obj)]

    [e (raise-user-error "Não implementado: " e)]))

; --- Execução de Declarações ---

; Executa atribuição, atualizando o valor de uma variável
(define (result-of stmt Δ)
  (match stmt
    [(ast:assign (ast:var x) e)
     (setref! (apply-env Δ x) (value-of e Δ))]

    ; Exibe o resultado da expressão
    [(ast:print e) (displayln (value-of e Δ))]

    [(ast:return e) (value-of e Δ)]

    ; Executa um bloco de comandos sequencialmente
    [(ast:block stmts) (for-each (lambda (s) (result-of s Δ)) stmts)]

    ; Condicional: executa s1 se verdadeiro, caso contrário s2
    [(ast:if-stmt e s1 s2) (if (value-of e Δ) (result-of s1 Δ) (result-of s2 Δ))]

    ; Laço while: repete enquanto a condição for verdadeira
    [(ast:while e s) (when (value-of e Δ)
                       (result-of s Δ)
                       (result-of stmt Δ))]

    ; Declaração local: estende o ambiente e executa
    [(ast:local-decl (ast:var x) s) (result-of s (extend-env x (newref 'null) Δ))]

    ; Avalia e aplica método ao objeto
    [(ast:send e (ast:var mth) args)
     (let* ([args-with-value (map-value-of args Δ)]
            [obj (value-of e Δ)])
       (apply-method (find-method (object-class-name obj) mth) obj args-with-value))]

    ; Avalia e aplica método da superclasse
    [(ast:super (ast:var c) args)
     (let* ([args-with-value (map-value-of args Δ)]
            [obj (apply-env Δ "self")])
       (apply-method (find-method (apply-env Δ "super") c) obj args-with-value))]
      
    [e (raise-user-error "Não implementado: " e)]))

; --- Gerenciamento de Ambiente e Classes ---

; Combina os métodos da classe e superclasse
(define (combine-methods m-decls super-name fields)
  (let ([super-methods (class-method-env (find-class super-name))]
        [self-methods (map (lambda (m-decl) (create-method super-name fields m-decl)) m-decls)])
    (append self-methods super-methods)))

; Cria um método com a superclasse e campos
(define (create-method super-name fields m-decl)
  (let ([method-name (ast:var-name (ast:method-name m-decl))]
        [method-vars (map ast:var-name (ast:method-params m-decl))]
        [method-body (ast:method-body m-decl)])
    (list method-name (method method-vars method-body super-name fields))))

; Extrai nomes dos campos (strings ou variáveis AST)
(define (get-field-names fields)
  (map (lambda (field)
         (if (string? field) field
             (ast:var-name field))) fields))

; Encontra uma classe no ambiente, levantando erro se não encontrada
(define (find-class class-name [raise-error? #t])
  (let ([maybe-pair (assoc class-name class-env)])
    (if maybe-pair
        (cdr maybe-pair)
        (if raise-error?
            (raise-user-error "Classe não encontrada: " class-name) #f))))

; Adiciona uma nova classe ao ambiente de classes
(define (add-class class-name class-list)
  (unless (find-class class-name #f)
    (set! class-env (cons (cons class-name class-list) class-env))))

; Verifica se uma classe existe no ambiente
(define (class-exists? class-name)
  (find-class class-name #f))

; Combina campos da superclasse e classe atual
(define (append-field-names super-fields self-fields)
  (foldr (lambda (field acc)
           (if (member field acc)
               (cons (string-append field "%1") acc)
               (cons field acc))) self-fields super-fields))

; --- Manipulação de Métodos ---

; Aplica um método ao objeto e estende o ambiente com variáveis
(define (apply-method method self args)
  (let* ([args-with-refs (map newref args)]
         [extended-env (extend-env "self" self (extend-env "super" (method-super-name method) empty-env))]
         [method-env (assign-vars (method-fields method) (object-fields self) extended-env)]
         [final-env (assign-vars (method-vars method) args-with-refs method-env)])
    (result-of (method-body method) final-env)))

; Atribui valores a variáveis no ambiente
(define (assign-vars vars values env)
  (foldl (lambda (var val acc-env)
           (extend-env var val acc-env)) env vars values))

; Encontra um método pelo nome, buscando na classe e superclasses
(define (find-method class-name method-name)
  (define (search-method class-name)
    (let ([class (find-class class-name)])
      (let ([maybe-pair (assoc method-name (class-method-env class))])
        (if maybe-pair
            (cadr maybe-pair)
            (if (class-super-name class)
                (search-method (class-super-name class))
                (raise-user-error "Método não encontrado: " method-name)))))) (search-method class-name))

; --- Avaliação do Programa ---

; Avalia o programa após declarar classes e executar o bloco de comandos
(define (value-of-program prog)
  (empty-store)
  (match prog
    [(ast:prog decls stmt)
     (begin
       (add-class "object" (class #f '() '()))
       (for-each (lambda (decl)
                   (let* ([class-name (ast:var-name (ast:decl-name decl))]
                          [super-name (ast:var-name (ast:decl-super decl))]
                          [super-fields (class-field-names (find-class super-name))]
                          [fields (append-field-names super-fields (get-field-names (ast:decl-fields decl)))]
                          [methods (combine-methods (ast:decl-methods decl) super-name fields)]
                          [class (class super-name fields methods)])
                     (add-class class-name class))) decls)
       (result-of stmt init-env))]))
