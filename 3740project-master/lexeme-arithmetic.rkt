#lang racket
;code from rosettacode.org
;this is what we used to base our undersanding of how to aproach the rest of the project
;we tried to build our own lexer/parser but were not making good progress so made use of the
;lexer and parser tools through racket.

(require parser-tools/yacc parser-tools/lex
         (prefix-in ~ parser-tools/lex-sre))
(require "input-output.ss")
(define-tokens value-tokens (NUM))
(define-tokens var-tokens (VAR))
(define-tokens end-tokens (ENDIF))
(define-empty-tokens op-tokens (OPEN CLOSE IF ELSEIF DEFINEVAR DEFINEFUNC ENDFUNC DO TO STEPSIZE INPUT OUTPUT THEN FOR = + - * / == <> < > <= >= EOF NEG))
 
(define base-lex
  (lexer [(eof) 'EOF]
         
         [whitespace (base-lex input-port)]
         [(~or "+" "-" "*" "/") (string->symbol lexeme)]
         [(~or "==" "<>" "<" ">" "<=" ">=") (string->symbol lexeme)]
         ["(" 'OPEN]
         [")" 'CLOSE]
         ["input" 'INPUT]
         ["output" 'OUTPUT]
         ["endif" (token-ENDIF (string->symbol lexeme))]
         ;["#definefunc" 
         [(~: (~+ numeric) (~? (~: #\. (~* numeric))))
          (token-NUM (string->number lexeme))]
         [(~+ alphabetic) (token-VAR (string->symbol lexeme))]
         ))
                  
(define parse
  (parser [start E] [end EOF]
          [tokens value-tokens op-tokens var-tokens end-tokens]
          [error void]
          [precs (left == <> < > <= >=) (left IF FOR THEN ELSEIF) (left - +) (left * /) (left NEG)]
          [grammar (E [(NUM) $1]
                      [(VAR) $1]
                      [(E == E) (equal? $1 $3)]
                      [(E <> E) (not(eq? $1 $3))]
                      [(E < E) (< $1 $3)]
                      [(E > E) (> $1 $3)]
                      [(E <= E) (<= $1 $3)]
                      [(E >= E) (>= $1 $3)]
                      [(E + E) (+ $1 $3)]
                      [(E - E) (- $1 $3)]
                      [(E * E) (* $1 $3)]
                      [(E / E) (/ $1 $3)]
                      [(INPUT E) (input $2)]
                      ([IF E THEN E ENDIF] (if (eq? #t $2) $4 '()))
                      ([IF E THEN E ELSEIF E] (if (eq? #t $2) $4 $6))
                      ([FOR E TO E STEPSIZE E DO E] '())
                      ([E = E] '())
                      ([OUTPUT E] '())
                      ([DEFINEVAR E] '())
                      ([DEFINEFUNC E ENDFUNC] '())
                      [(- E) (prec NEG) (- $2)]
                      [(OPEN E CLOSE) $2])
                   ]))

(define (myeval expr)
  (define i (open-input-string expr))
  (displayln (parse (lambda()(base-lex i))))

  )

(define (uofl)
   (display "UofL>")                 ; print a prompt
   (let ((expr (read-line))); read an expression, save it in expr
      (cond ((equal? expr "exit")        ; user asked to stop?
             (displayln "exiting"))
            (else
             (myeval expr)
             (set! expr "")
             (uofl)))))

(define (math-eval expr)
  (define i (open-input-string expr))
  ((parse (lambda() (base-lex i)))))

(provide math-eval)
 
