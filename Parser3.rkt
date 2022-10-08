#lang racket
(require "scanner.rkt")
(define (demo-scanner s)
  (begin
    (unless (input-ready? s) (display "Enter any text:\n"))
    (cond [(number? (peek s)) (print (string-append "handling a number: "
                                                    (number->string (pop! s))))]
          [(string? (peek s)) (print (string-append "handling a string: " 
                                                    (pop! s)))]
          [(eof-object? (peek s)) (print "At end-of-input")]
          [else (print (string-append "Impossible?!?!?  some other value: " (pop! s)))])
    (display "\n")
    (when (and (input-ready? s) (not (eof-object? (peek s)))) 
      (demo-scanner s)) ;; loop.
    ))
(provide (all-defined-out))
(define (parse! s)
  ; We use recursive-descent parsing.
  (cond [(string? s) (parse! (create-scanner s))]   ; overload for convenience:  handle scanner *or* string.
        [(number? (peek s)) (pop! s)]
        [(string=? "<" (peek s))
         (let* {[_ (pop! s)]   ; consume the "<" from front of `s`
                [the-inside-expr (parse! s)]
                [_ (pop! s)]  ; the closing-bracket
                }
           (make-paren the-inside-expr))]
        [(string=? "#" (peek s))
         (let* {[open-hash (pop! s)]
                [lefty  (parse! s)]
                [_ (if (not (member? (peek s) OPS))
                       (error 'parse "Unknown op " (peek s))
                       'keep-on-going)]
                [op     (pop! s)]
                [righty (parse! s)]
                [close-hash (pop! s)]
                }
           (make-binop lefty op righty))]
        [(string=? "even" (peek s))
         (let* {[_ (pop! s)]   ; throw away the opening "even"
                [_ (pop! s)]   ; throw away the opening "?"
                [the-test (parse! s)]
                [_ (pop! s)]   ; discard "dope"
                [the-even-ans (parse! s)]
                [_ (pop! s)] ; throw away the "nope"
                [the-odd-ans  (parse! s)]
                [_ (pop! s)] ; throw away "dawg"
                }
           (make-parity the-test the-even-ans the-odd-ans))]
        [else (error 'parse! (format "syntax error -- something has gone awry!  Seeing ~v" (peek s)))]))
