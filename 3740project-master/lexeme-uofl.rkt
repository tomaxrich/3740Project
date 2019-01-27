#lang racket
;this is from rosettacode.org
;it works perfectly, so it will probably be used for demoing
;or for the test cases that are needed.

;reader
(require parser-tools/yacc parser-tools/lex
         (prefix-in ~ parser-tools/lex-sre))
(require "lexeme-arithmetic.rkt")
 
(define-tokens value-tokens (NUM))
(define-tokens var-tokens (VAR))
(define-tokens op-tokens (OP))
(define-tokens expr-tokens (ARITH))
(define-empty-tokens special-tokens (INPUT OUTPUT FOR IF OPEN CLOSE EOF))


(define lex
  (lexer [(eof) 'EOF]
         [whitespace (lex input-port)]
         ["if" 'IF]
         ["for" 'FOR]
         ["input" 'INPUT]
         ["output" 'OUPUT]
         ["(" 'OPEN]
         [")" 'CLOSE]
         ["I" error]
         ["J" error]
         [(~: (~+ numeric) (~? (~: #\. (~* numeric))))
          (token-NUM (string->number lexeme))]
         [(~or "+" "-" "*" "/" "^" "==" "<=" ">=" "<>" "<" ">") (token-OP (string->symbol lexeme))]
         [(~+ alphabetic) (token-VAR (string->symbol lexeme))]
        
         [(~: 
           (~:(~+ numeric) (~? (~: #\. (~* numeric))))
           (~or "+" "-" "*" "/" "^" "==" "<=" ">=" "<>" "<" ">")
           (~:(~+ numeric) (~? (~: #\. (~* numeric)))))
           (token-ARITH lexeme)]
           ))

(define parse
  (parser [start E] [end EOF]
          [tokens value-tokens var-tokens expr-tokens op-tokens special-tokens]
          [error void]
          
          [grammar (E
                      [(NUM) $1] ;just a number, display the number
                      [(VAR) $1] ;DISPLAY VARIABLE
                      [(IF) "if"];give it to the if thing
                      [(FOR) "for"];give it to the for thing
                      [(INPUT) "input"];
                      [(E OP ARITH) (lex $1)]
                      [(ARITH) (math-eval $1))]
                     )
                   ]
          ))
 
(define (eval expr)
  (define i (open-input-string expr))
  (displayln (parse (lambda() (lex i)))
             )
  )
 
(define (uofl)
   (display "UofL>")                 ; print a prompt
   (let ((expr (read-line)))              ; read an expression, save it in expr
      (cond ((equal? expr "exit")        ; user asked to stop?
             (display "exiting")
             (newline))
            (else
             (eval expr)
             (uofl)))))


