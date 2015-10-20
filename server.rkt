(define (start req)
  (response/xexpr
   '(html (head (title "Racket Heroku App"))
          (body (h1 "Welcome to the LooLooZoo!")))))

(define port (if (getenv "PORT")
                 (string->number (getenv "PORT"))
                 4000))

(serve/servlet start
               #:servlet-path "/"
               #:listen-ip #f
               #:port port
               #:command-line? #t)
