#lang racket
(require (prefix-in : parser-tools/lex-sre))
(require parser-tools/lex)
(require parser-tools/lex-sre)


(define make_token
  (lexer
   [(eof) '()]

   [(:or "write")
    (cons `write
          (make_token input-port))]
   [(:or "read")
    (cons `read
          (make_token input-port))]
     
   [(:+ (:or (char-range #\a #\z)(char-range #\A #\Z)))
    (cons `ID
              (make_token input-port))]
   [#\(
    (cons `LPAR
          (make_token input-port))]
   [#\)
    (cons `RPAR
          (make_token input-port))]
   [#\-
    (cons `-
          (make_token input-port))]
   [#\+
    (cons `+
     (make_token input-port))]
   [#\*
    (cons `*
     (make_token input-port))]
   [#\/
    (cons `/
     (make_token input-port))]
   [(:+ (:or #\: #\=))
        (cons `:=
              (make_token input-port))]
   [(:+ (:or #\$ #\$))
        (cons `$$
              (make_token input-port))]
   [#\$
    (cons `(END, (string->symbol lexeme))
          (make_token input-port))]
   
   [(:: (:? #\-)(:+ (char-range #\0 #\9)))
        (cons `INT
             (make_token input-port))]

   [whitespace (make_token input-port)]
   ))

;(define tokens(make_token(open-input-file "input02.txt"))) ;testing
;tokens  ;testing
  
(define (RPAR tokens)
  [cond
    [(equal? (first tokens) `RPAR) (rest tokens)]
  [else (print "2")]])

(define (ass_op tokens)
  [cond
    [(equal? (first tokens) `:=) (rest tokens)]])

(define (ID tokens)
  [cond
    [(equal? (first tokens) `ID) (rest tokens)]])

(define (done tokens)
  [cond
    [(equal? (first tokens) `$$) (printf "accept \n")]])

(define (match expected tokens)
  (cond
    [(equal? (first tokens) expected)
      (rest tokens)]
    [else (error"Syntax Error")]))

(define (program tokens)
    (cond
      [(equal? (first tokens)`ID) (done(stmt_list tokens))]
      [(equal? (first tokens)`read) (done(stmt_list tokens))]
      [(equal? (first tokens)`write) (done(stmt_list tokens))]
      [(equal? (first tokens)`$$) (print"accept")]
      [else (error"Syntax Error")]))

(define (stmt_list tokens)
    (cond
      [(equal? (first tokens)`ID) (stmt_list(stmt tokens))]
      [(equal? (first tokens)`read) (stmt_list(stmt tokens))]
      [(equal? (first tokens)`write) (stmt_list(stmt tokens))]
      [(equal? (first tokens)`$$) tokens]
      [else (error "Syntax Error")]))

(define (stmt tokens)
    (cond
      [(equal? (first tokens)'ID) (expr (match `:= (match `ID tokens)))]
      [(equal? (first tokens)`read) (match `ID (match `read tokens))]
      [(equal? (first tokens)`write) (expr (match `write tokens))]
      [else (error"Syntax Error")]))

(define (expr tokens)
    (cond
      [(equal? (first tokens)`ID)(term_tail (term tokens))]
      [(equal? (first tokens)`INT)(term_tail (term tokens))]
      [(equal? (first tokens)`LPAR)(term_tail (term tokens))]
      [else (error"Syntax Error")]))

(define (term_tail tokens)
    (cond
      [(equal? (first tokens)`+ )(term_tail (term (add_op tokens)))]
      [(equal? (first tokens)`- )(term_tail (term (add_op tokens)))]
      [(equal? (first tokens)`RPAR) tokens]
      [(equal? (first tokens)`ID) tokens]
      [(equal? (first tokens)`read) tokens]
      [(equal? (first tokens)`write) tokens]
      [(equal? (first tokens)`$$) tokens]
      [else (error"Syntax Error")]))

(define (term tokens)
    (cond
      [(equal? (first tokens)`ID)(factor_tail (factor tokens))]
      [(equal? (first tokens)`INT)(factor_tail(factor tokens))]
      [(equal? (first tokens)`LPAR) (factor_tail(factor tokens))]
      [else (error"Syntax Error")]))

(define (factor_tail tokens)
    (cond
      [(equal? (first tokens)`*) (factor_tail (factor(mult_op tokens)))]
      [(equal? (first tokens)`/) (factor_tail (factor(mult_op tokens)))]
      [(equal? (first tokens) `+) tokens]
      [(equal? (first tokens) `-) tokens]
      [(equal? (first tokens)`RPAR) tokens]
      [(equal? (first tokens)`ID) tokens]
      [(equal? (first tokens)`read) tokens]
      [(equal? (first tokens)`write) tokens]
      [(equal? (first tokens)`$$) tokens]
      [else (error"Syntax Error")]))

(define (factor tokens)
   (cond
    [(equal? (first tokens) `ID) (match `ID tokens)]
    [(equal? (first tokens) `INT) (match `INT tokens)]
    [(equal? (first tokens) `LPAR) (match `RPAR (expr (rest tokens)))]
    [else (error"Syntax Error")]))

(define (add_op tokens)
    (cond
     [(equal? (first tokens) `+) (rest tokens)]
     [(equal? (first tokens) `-) (rest tokens)]
     [else (error"Syntax Error")]))

(define (mult_op tokens)
     (cond
    [(equal? (first tokens) `*) (rest tokens)]
    [(equal? (first tokens) `/) (rest tokens)]
    [else (error"Syntax Error")]))

(define (parse input-file)
 (program (make_token (open-input-file input-file)))) 

(parse "input01.txt")
(parse "input02.txt")
(parse "input03.txt")
(parse "input04.txt")
(parse "input05.txt")