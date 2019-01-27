#lang racket

(define (uoflmath)
   (display "UofL>")                 ; print a prompt
   (let ((expr (read)))              ; read an expression, save it in expr
      (cond ((eq? expr 'exit)        ; user asked to stop?
             (display "exiting")
             (newline))
            (else
             (write (arithmetic-eval expr))
             (newline)
             (uoflmath)))))

(define (arithmetic-eval expr) 
  (cond 
        ((number? expr);if the expression is just a number
         expr);return the number

        ((pair? expr) ;evaluate the expression
         (single-op-eval expr))

        ))
(provide arithmetic-eval)


(define (single-op-eval expr) ;evaluates expression of type <val> <op> <val>

  (let ((op (cadr expr));second element of the 'list' that is the expression
        (val1 (arithmetic-eval (car expr)))
        (val2 (arithmetic-eval (caddr expr))))

    (cond ((eq? op '+)
            (+ val1 val2))
           ((eq? op '-)
            (- val1 val2))
           ((eq? op '*)
            (* val1 val2))
           ((eq? op '/)
            (/ val1 val2))
           ((eq? op '^)
            '(power val1 val2))
           (else
            (error "Invalid operation in expr:" expr)))))

