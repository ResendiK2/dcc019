#lang racket

(require dcc019/util/env
         dcc019/util/memory
         dcc019/exercise/iclasses/ast)

(provide value-of-program)

; value-of :: Exp -> ExpVal
(define (value-of exp Δ)
  (match exp
    [(ast:int v) v]
    [(ast:bool v) v]
    [(ast:dif e1 e2) (- (value-of e1 Δ) (value-of e2 Δ))]
    [(ast:zero? e) (zero? (value-of e Δ))]
    [(ast:not e) (not (value-of e Δ))]
    [(ast:var v) (apply-env Δ v)]
    [(ast:if e1 e2 e3) 
     (if (value-of e1 Δ) 
         (value-of e2 Δ) 
         (value-of e3 Δ))]
    [(ast:let (ast:var x) e1 e2) 
     (value-of e2 (extend-env x (value-of e1 Δ) Δ))]
    [(ast:send e (ast:var mth) args) 
     (display "send expression unimplemented")]
    [(ast:super (ast:var c) args) 
     (display "super expression unimplemented")]
    [(ast:self) 
     (display "self expression unimplemented")]
    [(ast:new (ast:var c) args) 
     (display "new expression unimplemented")]
    [e (raise-user-error "unimplemented-construction: " e)]))

; Função para processar declarações e comandos
(define (result-of stmt Δ)
  (match stmt
    [(ast:assign (ast:var x) e)
     (let ((new-val (value-of e Δ)))
       (extend-env x new-val Δ))]

    [(ast:print e)
     (let ((val (value-of e Δ)))
       (display val)
       (newline)
       Δ)]

    [(ast:block stmts)
     (foldl (lambda (s env) (result-of s env)) Δ stmts)]
    [(ast:if-stmt e s1 s2)
     (if (value-of e Δ)
         (result-of s1 Δ)
         (result-of s2 Δ))]

    [(ast:return e)
     (value-of e Δ)]

    [(ast:while e s)
     (while-loop e s Δ)]

    [(ast:local-decl (ast:var x) s)
     (let ((new-env (extend-env x 0 Δ)))
       (result-of s new-env))]

    [(ast:send e (ast:var mth) args) 
     (display "send command unimplemented")]
     
    [(ast:super (ast:var c) args) 
     (display "super command unimplemented")]
    [e (raise-user-error "unimplemented-construction: " e)]))

; Função para tratar loops while
(define (while-loop e s Δ)
  (let ((condition (value-of e Δ)))
    (if condition
        (let ((new-env (result-of s Δ)))
          (while-loop e s new-env))
        (displayln "Loop encerrado."))))

; Função para avaliar um programa
(define (value-of-program prog)
  (match prog
    [(ast:prog decls stmt)
     (begin
       ; you must collect all the classes declared and building its respectively environment
       ; execute the prog expression in the correct environment
       (result-of stmt init-env))]))

