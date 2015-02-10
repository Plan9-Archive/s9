(define say-hello
  (λ () (print "hello, world")))

(define map1
  (λ (f lst)
    (if (eq? lst '())
        '()
        (cons (f (car lst)) (map1 f (cdr lst))))))
