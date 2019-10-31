#lang racket
(require db)

(define pgc
    (postgresql-connect #:user "drichter"
                        #:database "drichter"
                        #:password "drichter"
                        #:port 6432))

