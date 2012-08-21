#lang scheme

;; from ftp://ftp.cs.utexas.edu/pub/garbage/cs345/schintro-v14/schintro_114.html

;(define (rep-loop)
;    (display "repl>") ; print a prompt
;    (write (eval (read))) ; read expr, pass to eval, write result
;    (rep-loop)) ; loop (tail-recursive call) to do it again
    
;; that loops infinitely, w/o "halt" symbol

;(define (rep-loop)
;    (display "repl>")
;    (let ((expr (read)))
;        (cond ((eq? expr 'halt)
;            (display "exiting REPL")
;            (newline))
;        (#t
;            (write (eval expr))
;            (newline)
;            (rep-loop)))))
;; problem: should ensure that the halt comm doesn't have the syntax of any other expr in lang
;; but OK for now

(define (rep-loop evaluator)
    (display "repl>")
    (let ((expr(read)))
        (cond ((eq? expr 'exit)
            (display "exiting REPL")
            (newline))
        (#t
            (write (evaluator expr))
            (rep-loop evaluator)))))

;;read: uses recursion to construct nested data structures while reading through char input L -> R
 ;;
 ;;micro-read : should only handle a few basic types:: symbols, non-neg integers, and lists; + little error-checking
 ;;so read must recognize nested structures, given read-token which reads...tokens...

(define left-paren-token (list '*left-parenthesis-token*)) 
(define right-paren-token (list '*right-parenthesis-token*))

(define (read-token)
    (let ((first-char (read-char)))
        (cond 
            ((char-whitespace? first-char)
                (read-token))
            ((eq? first-char #\()
             left-paren-token )
            ((eq? first-char #\))
             right-paren-token )
            ((char-alphabetic? first-char)
                (read-identifier first-char))
            ((char-numeric? first-char)
                (read-number first-char))
            (else
                (error "illegal lexical syntax" )))))
                
;; some of the predicates in read-token must be defined by programmer

;;; whitespace? -- checks whether char is space or newline, called char-whitespace?
(define (char-whitespace? char)
    (or (eq? char #\space)
        (eq? char #\newline)))

;(define left-paren-token (list '*left-parenthesis-token*)) 
;(define right-paren-token (list '*right-parenthesis-token*))




;; read-identifier: if read a letter, this is a symbol, so finish reading it w/ this
;; read-identifier reads an identifier and returns a symbol to represent it

(define (read-identifier chr)
    (define (read-identifier-helper list-so-far)
        (let ((next-char (peek-char)))
            (if (or (char-alphabetic? next-char))
                (char-numeric? next-char))
            (read-identifier-helper (cons (read-char) list-so-far))
            (reverse list-so-far))))
            
            ;; call r-i-h to accumulate chars in the identifier, then convert that to 
            ;; a string obj and convert THAT to a symbol obj
            ;; n.b. string-> symbol ensures only one symbol with a given printnm str
            ;; is ever constructed -- so there are no duplicates

; yes?? or is this an example?
(string->symbol (list->string (read-identifier-helper (list chr))))


;; read-number reads a seq of digits and constructs a Scheme number obj to rep it
;; if next char is a digit, it cons's it onto a list of numbers read-so-far -- otherwise, reverses list of digits, converts it to a str, and converts that to a scheme number obj

(define (read-number chr)
    (define (read-number-helper list-so-far)
        (let ((next-char (peek-char)))
            (if (char-numeric? next-char)
                (read-number-helper (cons (read-char) list-so-far))
                (reverse list-so-far)))))
                
(string->number (list->string (read-number-helper (list chr))))

;; implementing the read procedure

;;; easy after implementing read-token -- we'll call our read 'micro-read', though

;; Simplified version of read for subset of Scheme s-expr syntax
(define (micro-read)
    (let ((next-token (read-token))) ; added another paren at end here
        (cond ((token-leftpar? next-token)
            (read-list '()))
           (else
            next-token))))
  
(define (read-list list-so-far)
    (let ((token (micro-read-token)))
        (cond ((token-rightpar? token)
            (reverse list-so-far))
         ((token-leftpar? token)
             (read-list (cons (read-list '()) list-so-far)))
         (else
             (read-list (cons token list-so-far))))))
        