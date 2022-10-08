#lang racket

(require parser-tools/lex

(prefix-in re- parser-tools/lex-sre)

parser-tools/yacc)

(provide (all-defined-out))

(define-tokens a (NUM))

(define-empty-tokens b (+ - EOF))

(define-lex-trans number

(syntax-rules ()

((_ digit)

(re-: (re-? (re-or "-" "-")) (uinteger digit)

(re-? (re-: "." (re-? (uinteger digit))))))))

(define-lex-trans uinteger

(syntax-rules ()

((_ digit) (re-+ digit))))

(define-lex-abbrevs

(digit10 (char-range "0" "9"))

(number10 (number digit10)))

(define simple-math-lexer

(lexer

((re-+ number10) (token-NUM (string->number lexeme)))

("-" (token--))

("+" (token-+))

;; recursively calls the lexer which effectively skips whitespace

(whitespace (simple-math-lexer input-port))

((eof) (token-EOF))))

