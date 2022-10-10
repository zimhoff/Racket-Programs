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

(define tokens(make_token(open-input-file "input01.txt"))) ;testing
tokens  ;testing

;(define input-port (open-input-file fileName))
  
(define (RPAR tokens)
  [cond
    [(equal? (first tokens) ")" (rest tokens))]])

(define (ass_op tokens)
  [cond
    [(equal? (first tokens) ":=" (rest tokens))]])

(define (ID tokens)
  [cond
    [(equal? (first tokens) "ID" (rest tokens))]])

(define (error)
  (printf "error"))

;(define ($$ tokens)
  ;[cond
    ;[(equal? (first tokens) "$$" (rest tokens))]])

(define (match expected tokens)
  (cond
    [(equal? (first tokens) expected)
      (rest tokens)]
     [else print"error"]))
    ;[(equal? (first tokens) tokens (rest tokens))]

(define (program tokens)
    (cond
    ;(case (first(first tokens))
      ;[(ID read write $$)(stmt_list (rest tokens))]
      [(equal? (first tokens)`ID) (stmt_list tokens)]
      [(equal? (first tokens)`read) (stmt_list tokens)]
      [(equal? (first tokens)`write) (stmt_list tokens)]
      [(equal? (first tokens)`$$) (stmt_list tokens)]
      [(equal? (first tokens)`$$) (match `$$ tokens)]
      [else (print "1")]))

(define (stmt_list tokens)
    (cond
    ;(case (first (first tokens))
      ;[(ID read write :=) (stmt_list(stmt (rest tokens)))]
      ;[($$) tokens]
      [(equal? (first tokens)`ID) (stmt_list(stmt tokens))]
      [(equal? (first tokens)`read) (stmt_list(stmt tokens))]
      [(equal? (first tokens)`write) (stmt_list(stmt tokens))]
      [(equal? (first tokens)`$$) (stmt_list(stmt tokens))]
      [(equal? `$$ (first tokens)) tokens]
      [else ( print"1")]))

(define (stmt tokens)
    (cond
    ;(case (first (first tokens))
      ;[(ID)(expr(ass_op(rest tokens)))]
      ;[(read)(ID(rest tokens))]
      ;[(write)(expr(rest tokens))]
      [(equal? (first tokens)'ID) (expr (match `:= (match `ID tokens)))]
      [(equal? (first tokens)`read) (match `ID (match `read tokens))]
      [(equal? (first tokens)`write) (expr (match `write tokens))]
      [else (print "5")]))

(define (expr tokens)
    (cond
    ;(case (first(first tokens))
      ;[(ID INT RPAR)(term_tail (term(rest tokens)))]
      [(equal? (first tokens)`ID)(term_tail (term tokens))]
      [(equal? (first tokens)`INT)(term_tail (term tokens))]
      [(equal? (first tokens)`LPAR)(term_tail (term tokens))]
      [else (error)]))

(define (term_tail tokens)
    (cond
    ;(case (first(first tokens))
      ;[(+ -)(term_tail (term(rest tokens)))]
      ;[(RPAR ID read write $$) tokens]
      [(equal? (first tokens)`+ )(term_tail (term tokens))]
      [(equal? (first tokens)`- )(term_tail (term tokens))]
      [(equal? (first tokens)`RPAR) tokens]
      [(equal? (first tokens)`ID) tokens]
      [(equal? (first tokens)`read) tokens]
      [(equal? (first tokens)`write) tokens]
      [(equal? (first tokens)`$$) tokens]
      [else (error)]))

(define (term tokens)
    (cond
    ;(case (first(first tokens))
     ; [(ID INT LPAR)(factor_tail (factor(rest tokens)))]
      [(equal? (first tokens)`ID)(factor_tail (factor tokens))]
      [(equal? (first tokens)`INT)(factor_tail(factor tokens))]
      [(equal? (first tokens)`LPAR) (factor_tail(factor tokens))]
      [else (error)]))

(define (factor_tail tokens)
    (cond
    ;(case (first (first tokens))
      ;[(* /)(factor_tail (factor(rest tokens)))]
      ;[(+ - RPAR ID read write $$)tokens]
      [(equal? (first tokens)`*) (factor_tail (factor(rest tokens)))]
      [(equal? (first tokens)`/) (factor_tail (factor(rest tokens)))]
      [(equal? (first tokens) `+) (rest tokens)]
      [(equal? (first tokens) `-) (rest tokens)]
      [(equal? (first tokens)`RPAR) tokens]
      [(equal? (first tokens)`ID) tokens]
      [(equal? (first tokens)`read) tokens]
      [(equal? (first tokens)`write) tokens]
      [(equal? (first tokens)`$$) tokens]
      [else (error)]))

(define (factor tokens)
   (cond
   ;(case (first(first tokens))
     ;[(LPAR) (RPAR(expr (rest tokens)))]
     ;[(ID INT)(rest tokens)]
    [(equal? (first tokens) `LPAR) (RPAR(expr (rest tokens)))]
    [(equal? (first tokens) `ID) (rest tokens)]
    [(equal? (first tokens) `INT) (rest tokens)]
    [else(error)]))

(define (add_op tokens)
    (cond
    ;(case (first(first tokens))
     ;[(+ -) (rest tokens)]
     [(equal? (first tokens) `+) tokens]
     [(equal? (first tokens) `-) tokens]
     [else (error)]))

(define (mult_op tokens)
     (cond
     ;(case first tokens)
    ;[(* /) (rest tokens)]
    [(equal? (first tokens) `*) tokens]
    [(equal? (first tokens) `/) tokens]
    [else (error)]))

(define (parse input-file)
 (program (make_token (open-input-file input-file)))) 

;(define (parse fileName)
  ;(make_token (open-input-file fileName))))
;(parse "input01.txt")

  ;(define (parse tokens)
   ;(program tokens)
    ;(define tokens (tokenizer(open-input-file "input01.txt"))))


;procedure match(expected)
;if input token = expected then consume input token()
;else parse error
