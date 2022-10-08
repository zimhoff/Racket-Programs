#lang racket
(require (prefix-in : parser-tools/lex-sre))
(require parser-tools/lex)
(require parser-tools/lex-sre)


(define tokenizer
  (lexer
   [(eof) '()]

   [(:or "write")
    (cons ` (write)
          (tokenizer input-port))]
   [(:or "read")
    (cons ` (read)
          (tokenizer input-port))]
     
   [(:+ (:or (char-range #\a #\z)(char-range #\A #\Z)))
    (cons `(ID)
              (tokenizer input-port))]
   [#\(
    (cons `(LPAR)
          (tokenizer input-port))]
   [#\)
    (cons `(RPAR)
          (tokenizer input-port))]
   [#\-
    (cons `(-)
          (tokenizer input-port))]
   [#\+
    (cons `(+)
     (tokenizer input-port))]
   [#\*
    (cons `(*)
     (tokenizer input-port))]
   [#\/
    (cons `(/)
     (tokenizer input-port))]
   [(:+ (:or #\: #\=))
        (cons `(:=)
              (tokenizer input-port))]
   [(:+ (:or #\$ #\$))
        (cons `($$)
              (tokenizer input-port))]
   [#\$
    (cons `(END, (string->symbol lexeme))
          (tokenizer input-port))]
   
   [(:: (:? #\-)(:+ (char-range #\0 #\9)))
        (cons `(INT)
             (tokenizer input-port))]
   
   
   
   [whitespace (tokenizer input-port)]
   ))
(define tokens(tokenizer(open-input-file "input02.txt"))) ;testing
tokens  ;testing

;(define input-port (open-input-file fileName))

;(define (parser input-file)
 ;(program (tokenizer (open-input-file inputFile)))
 ;)

;(define (syntaxError)
 ; (printf "Syntax error")
 ; (exit 0))
  
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
     ; [(equal? (first tokens) "$$" (rest tokens))]])


   (define (mult_op tokens)
  (cond
    [(equal? (first tokens) "*") (rest tokens)]
    [(equal? (first tokens) "/") (rest tokens)]
    [else (error)]))

  (define (add_op tokens)
    (cond
     [(equal? (first tokens) "+")]
     [(equal? (first tokens) "-")]
     [else (error)]))

  (define (factor tokens)
   (cond
    [(equal? (first tokens) "LPAR |(|") (RPAR(expr (rest tokens)))]
    [(equal? (first tokens) "ID")]
    [(equal? (first tokens) "INT")]
    [else(error)]))

  (define (factor_tail tokens)
    (cond
      [(equal? "*" (first tokens))(factor_tail (factor(rest tokens)))]
      [(equal? "/" (first tokens))(factor_tail (factor(rest tokens)))]
      [(equal? "RPAR"(first tokens)) tokens]
      [(equal? "ID" (first tokens)) tokens]
      [(equal? "read" (first tokens)) tokens]
      [(equal? "write" (first tokens)) tokens]
      [(equal? "$$" (first tokens)) tokens]
      [else (error)]))

  (define (term tokens)
    (cond
      [(equal? "ID" (first tokens))(factor_tail (factor(rest tokens)))]
      [(equal? "INT" (first tokens))(factor_tail(factor(rest tokens)))]
      [(equal? "LPAR |(|" (first tokens)(factor_tail(factor(rest tokens))))]
      [else (error "error")]))

  (define (term_tail tokens)
    (cond
      [(equal? "+" (first tokens))(term_tail (term(rest tokens)))]
      [(equal? "-" (first tokens))(term_tail (term(rest tokens)))]
      [(equal? "RPAR"(first tokens)) tokens]
      [(equal? "ID" (first tokens)) tokens]
      [(equal? "read" (first tokens)) tokens]
      [(equal? "write" (first tokens)) tokens]
      [(equal? "$$" (first tokens)) tokens]
      [else (error )]))

  (define (expr tokens)
    (cond
      [(equal? "ID" (first tokens))(term_tail (term(rest tokens)))]
      [(equal? "INT" (first tokens))(term_tail (term(rest tokens)))]
      [(equal? "RPAR" (first tokens))(term_tail (term(rest tokens)))]
      [else (error )]))

  (define (stmt tokens)
    (cond
      [(equal? "ID" (first tokens))(expr(ass_op(rest tokens)))]
      [(equal? "read" (first tokens))(ID(rest tokens))]
      [(equal? "write" (first tokens))(expr(rest tokens))]
      [else (error)]))

  (define (stmt_list tokens)
    (print "hi")
    (cond
      [(equal? "ID" (first tokens))(stmt_list(stmt(rest tokens)))]
      [(equal? "read" (first tokens))(stmt_list(stmt(rest tokens)))]
      [(equal? "write" (first tokens))(stmt_list(stmt(rest tokens)))]
      [(equal? "$$" (first tokens))(stmt_list(stmt(rest tokens)))]
      [else (error)]))

  (define (program tokens)
    (cond
      [(equal? "ID" (first(first tokens)))(done(print(tokens)))]
      [(equal? "read" (first(first tokens)))(done(print(tokens)))]
      [(equal? "write" (first(first tokens)))(done(print(tokens)))]
      [(equal? "$$" (first(first tokens)))(done(print(tokens)))]
      [else (error)]))

  (define (done tokens)
    (cond
     [(equal? "$$" (tokens))
     (printf "successful")]))

  (define (parse input-file)
 (program (tokenizer (open-input-file input-file))))

;(parse "input02.txt")

  ;(define (parse tokens)
    ;(program tokens)
    ;(define tokens (tokenizer(open-input-file "input02.txt"))))


#|
procedure match(expected)
if input token = expected then consume input token()
else parse error


– – this is the start routine:
procedure program()
case input token of
id, read, write, $$ :
stmt list()
match($$)
otherwise parse error


;procedure stmt list()
;case input token of
;id, read, write : stmt(); stmt list()
$$ : skip – – epsilon production
;otherwise parse error


;procedure stmt()
;case input token of
;id : match(id); match(:=); expr()
;read : match(read); match(id)
;write : match(write); expr()
;otherwise parse error

;procedure term tail()
;case input token of
;+, - : add op(); term(); term tail()
;), id, read, write, $$ :
skip – – epsilon production
;otherwise parse error

;procedure factor tail()
;case input token of
;*, / : mult op(); factor(); factor tail()
;+, -, ), id, read, write, $$ :
skip – – epsilon production
;otherwise parse error

|#