
;; an object!
(define (make-student name age)
  (lambda (msg . args)
    (cond
     ((eq? msg 'getName) name)
     ((eq? msg 'getAge) age)
     ((eq? msg 'setName) (set! name (car args)))
     ((eq? msg 'setAge) (set! age (car args)))
     (else 'unknownMethod))))

;; bad!
(define (fib n)
  (if (< n 2) n
      (+ (fib (- n 1)) (fib (- n 2)))))

(define fib-cache '())

;; memoized version
(define (fib2 n)
  (let ((val (assq n fib-cache)))
    (if val (cdr val)
        (let ((result (if (< n 2) n
                          (+ (fib2 (- n 1)) (fib2 (- n 2))))))
          (set! fib-cache (cons (cons n result) fib-cache))
          result))))


;; applicative vs. normal order

(define (die) (die))

(define (add2 a b)
  (+ (a) (b)))

;; can't do this:
;; (map '(lambda (a) (+ a 10))
;;      (infinite-list))


(define later)
(begin (display "outside delay")
       (set! later (delay
              (begin
                (display "in delay")
                1)))
       (display "after delay"))


(define promises (map (lambda (a b)
                        (delay (string-append a b)))
                      '("hello" "there") '("foo" "bar")))


(define kont '())
(cons 'top (call-with-current-continuation
            (lambda (k^) (set! kont k^))))


;; continuations to break/resume

(define escape)
(define RESUME)
(call/cc (lambda (k^) (set! escape k^)))

(define BREAK
  (lambda (msg)
    (call/cc
     (lambda  (k^)
       (set! RESUME k^)
       (escape msg)))))

;; (map (lambda (a)
;;        (BREAK (string-append "mid map: " (number->string a)))
;;        (+ 5 a))
;;      '(1 2 3))
