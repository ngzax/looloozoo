#lang racket

(require web-server/servlet
         web-server/servlet-env)

(define (start req)
  (response/xexpr
   '(html (head (title "LooLooZoo - A Racket Heroku App"))
          (body
            (h1 "Welcome to the LooLooZoo!")
            (h2 "Straight to you from Valley Forge and NEU.")))))

(define port (if (getenv "PORT")
                 (string->number (getenv "PORT"))
                 4000))

(serve/servlet start
               #:servlet-path "/"
               #:listen-ip #f
               #:port port
               #:command-line? #t)
