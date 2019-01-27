#lang racket

(require racket/include)
(require "arithmetic-eval.rkt")


(define ar-ops '(^ + / * -));list of valid arithmetic operators
(define rel-ops '(== <> <= >= < >));list of valid relational operators
(define eq-op '(=))
(define ops (append (append ar-ops rel-ops) '(=))) ;a list of all valid operators, including assignment

(define (uofl)
   (display "UofL>")                 ; print a prompt
   (let ((expr (read)))              ; read an expression, save it in expr
      (cond ((eq? expr 'exit)        ; user asked to stop?
             (display "exiting")
             (newline))
            (else
             (eval expr)
              (newline)
             (uofl)))))


;main evaluator
;decides what the expression is and sends it to the right place
(define (eval expr)
  (cond
    ;is a 'self evaluating' expression. just a number or a boolean (or a defined variable)
    ((self-eval? expr)
     (display expr)
     (newline))

    ;if the expression is a list, send it to the list evaluator
    ((pair? expr)
     (eval-list expr))
    
    ;if the expression is not of any recognized type
  (else(display "ERROR")
       (newline))
  )
)

(define (self-eval? expr)
  (or (number? expr) (boolean? expr)))

;for evaluating any list type expression (anything that isnt jsut a number or a variable inputted)
(define (eval-list expr)

  (cond
    ;if the expression is a math (numbers only type
    ((number?(car expr))
     (num-eval expr))

    ((eq? (car expr) 'definevari)
    (special expr))

  ((eq? (car expr) 'for)
  (iterative expr))

  ((eq? (car expr) 'if)
   (selection expr))

(else(display "ERROR")
       (newline))

    ))

(define (num-eval expr)
  (cond
    ;if the expression is of relational form
    ((member (cadr expr) rel-ops)
     (relational expr))

    ;if the expression is of assignment form
    ((member (cadr expr) eq-op)
     (assignment expr))

    ;else it is arithmetic
    ((member (cadr expr) ar-ops)
     (write (arithmetic-eval expr)))

      (else(display "ERROR")
       (newline))
    ))

(define (arithmetic expr)
  (display "arithmetic"))

(define (assignment expr)
  (display "assignment"))

(define (selection expr)
  (display "selection"))

(define (iterative expr)
  (display "iterative"))

(define (relational expr)
  (display "relational"))

(define (special expr)
  (display "special"))
