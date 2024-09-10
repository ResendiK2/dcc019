; Breno Lino Prado - 202265013AC
; Gabriel Arantes Resende Pereira - 202065126A

#lang racket

(require dcc019/util/env
         dcc019/util/memory
         dcc019/exercise/iclasses/ast)

(provide value-of-program)

; Estruturas principais da linguagem ICLASSES:
; - object: Representa um objeto com o nome da classe e os campos do objeto.
; - class: Representa uma classe com o nome da superclasse, nomes dos campos e ambiente dos métodos.
; - method: Representa um método com variáveis, corpo do método, nome da superclasse e campos.
(struct object (class-name fields))
(struct class (super-name field-names method-env))
(struct method (vars body super-name fields))

; Define o ambiente das classes como uma lista vazia inicialmente
(define class-env '())

; --- Cálculo de Valores ---

; Avalia uma lista de expressões no ambiente dado e retorna uma lista de valores resultantes.
(define (map-value-of exps Δ)
  (map (lambda (exp) (value-of exp Δ)) exps))

; Avalia a expressão
(define (value-of exp Δ)
  (match exp
    [(ast:int v) v]
    [(ast:bool v) v]
    [(ast:dif e1 e2) (- (value-of e1 Δ) (value-of e2 Δ))]
    [(ast:zero? e) (zero? (value-of e Δ))]
    [(ast:not e) (not (value-of e Δ))]
    [(ast:if e1 e2 e3) (if (value-of e1 Δ) (value-of e2 Δ) (value-of e3 Δ))]

    ; Avaliação de uma expressão let, que define uma variável e avalia uma expressão no novo ambiente
    [(ast:let (ast:var x) e1 e2)
     (let ([val (value-of e1 Δ)]) 
       (value-of e2 (extend-env x (newref val) Δ)))]

    [(ast:var v) (deref (apply-env Δ v))]

    ; Avaliação de uma chamada de método em um objeto, aplicando o método ao objeto e argumentos avaliados
    [(ast:send e (ast:var mth) args)
     (let* ([args-with-value (map-value-of args Δ)]
            [obj (value-of e Δ)])
       (apply-method (find-method (object-class-name obj) mth) obj args-with-value))]

    ; Avaliação de uma chamada de método na superclasse do objeto atual, aplicando o método ao objeto e argumentos avaliados
    [(ast:super (ast:var c) args)
     (let* ([args-with-value (map-value-of args Δ)]
            [obj (apply-env Δ "self")])
       (apply-method (find-method (apply-env Δ "super") (ast:var-name args)) obj args-with-value))]

    ; Avaliação da expressão self, que retorna o próprio objeto onde o método está sendo executado
    [(ast:self) (apply-env Δ "self")]

    ; Avaliação de uma expressão new, que cria um novo objeto da classe especificada e inicializa seus campos
    [(ast:new (ast:var c) args)
     (let* ([args-with-value (map-value-of args Δ)]
            [obj (let* ([class (find-class c)]
                        [field-names (class-field-names class)]
                        [fields (map (lambda (field-name) (newref null)) field-names)]) (object c fields))])
       (apply-method (find-method c "initialize") obj args-with-value) obj)]

    [e (raise-user-error "Não implementado: " e)]))

; --- Execução de Declarações ---

(define (result-of stmt Δ)
  (match stmt

    ; Atribuição: Calcula o valor da expressão e atualiza a variável no ambiente.
    [(ast:assign (ast:var x) e)
     (setref! (apply-env Δ x) (value-of e Δ))]

    ; Impressão: Calcula o valor da expressão e exibe o resultado.
    [(ast:print e) (displayln (format "Resultado: ~a" (value-of e Δ)))]
    
    [(ast:return e) (value-of e Δ)]

    ; Bloco: Executa cada comando no bloco sequencialmente dentro do mesmo ambiente.
    [(ast:block stmts) (for-each (lambda (s) (result-of s Δ)) stmts)]

    ; Condicional: Avalia a condição e executa o primeiro comando se verdadeira, caso contrário o segundo comando.
    [(ast:if-stmt e s1 s2) (if (value-of e Δ) (result-of s1 Δ) (result-of s2 Δ))]

    ; Laço While: Continua executando o comando enquanto a condição for verdadeira.
    [(ast:while e s) (when (value-of e Δ)
                       (result-of s Δ)
                       (result-of stmt Δ))]

    ; Declaração Local: Estende o ambiente com uma nova variável inicializada como 'null' e executa o comando.
    [(ast:local-decl (ast:var x) s) (result-of s (extend-env x (newref 'null) Δ))]

    ; Chamada de Método: Avalia os argumentos, encontra o método na classe do objeto e o aplica ao objeto.
    [(ast:send e (ast:var mth) args)
     (let* ([args-with-value (map-value-of args Δ)]
            [obj (value-of e Δ)])
       (apply-method (find-method (object-class-name obj) mth) obj args-with-value))]

    ; Chamada de Método na Superclasse: Avalia os argumentos, encontra o método na superclasse do objeto e o aplica. 
    [(ast:super (ast:var c) args)
     (let* ([args-with-value (map-value-of args Δ)]
            [obj (apply-env Δ "self")])
       (apply-method (find-method (apply-env Δ "super") c) obj args-with-value))]
      
    [e (raise-user-error "Não implementado: " e)]))

; --- Gerenciamento de Ambiente e Classes ---

; Combina os métodos da classe atual com os métodos herdados da superclasse.
(define (combine-methods m-decls super-name fields)
  (let ([super-methods (class-method-env (find-class super-name))]
        [self-methods (map (lambda (m-decl) (create-method super-name fields m-decl)) m-decls)])
    (append self-methods super-methods)))

; Cria um método a partir de sua declaração, incluindo informações sobre a superclasse e os campos.
(define (create-method super-name fields m-decl)
  (let ([method-name (ast:var-name (ast:method-name m-decl))]
        [method-vars (map ast:var-name (ast:method-params m-decl))]
        [method-body (ast:method-body m-decl)])
    (list method-name (method method-vars method-body super-name fields))))

; Extrai os nomes dos campos de uma lista de campos, convertendo strings e variáveis AST.
(define (get-field-names fields)
  (map (lambda (field)
         (if (string? field) field
             (ast:var-name field))) fields))

; Encontra uma classe pelo nome no ambiente de classes, com a opção de gerar um erro se não for encontrada.
(define (find-class class-name [raise-error? #t])
  (let ([maybe-pair (assoc class-name class-env)])
    (if maybe-pair
        (cdr maybe-pair)
        (if raise-error?
            (raise-user-error "Classe não encontrada: " class-name) #f))))

; Adiciona uma nova classe ao ambiente de classes, se ainda não estiver presente.
(define (add-class class-name class-list)
  (unless (find-class class-name #f)
    (set! class-env (cons (cons class-name class-list) class-env))))

; Verifica se uma classe existe no ambiente de classes.
(define (class-exists? class-name)
  (find-class class-name #f))

; Combina os nomes dos campos da superclasse e da classe.
(define (append-field-names super-fields self-fields)
  (foldr (lambda (field acc)
           (if (member field acc)
               (cons (string-append field "%1") acc)
               (cons field acc))) self-fields super-fields))

; --- Manipulação de Métodos ---

; Aplica um método a um objeto com uma lista de argumentos, estendendo o ambiente com variáveis de método e de objeto.
(define (apply-method method self args)
  (let* ([args-with-refs (map newref args)]
         [extended-env (extend-env "self" self (extend-env "super" (method-super-name method) empty-env))]
         [method-env (assign-vars (method-fields method) (object-fields self) extended-env)]
         [final-env (assign-vars (method-vars method) args-with-refs method-env)])
    (result-of (method-body method) final-env)))

; Atribui valores às variáveis no ambiente, retornando o ambiente estendido. 
(define (assign-vars vars values env)
  (foldl (lambda (var val acc-env)
           (extend-env var val acc-env)) env vars values))

; Encontra um método pelo nome, procurando na classe e suas superclasses.
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