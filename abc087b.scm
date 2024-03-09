(define-module abc087b
  (export abc087b))

(select-module abc087b)

(define (abc087b)
  (let ((a (string->number (read-line (current-input-port))))
        (b (string->number (read-line (current-input-port))))
        (c (string->number (read-line (current-input-port))))
        (x (string->number (read-line (current-input-port)))))
    (define (inner l m n ret)
      (cond ((> l a) ret)
            ((> m b) (inner (+ l 1) 0 n ret))
            ((> n c) (inner l (+ m 1) 0 ret))
            (else (if (= x (+ (* 500 l) (* 100 m) (* 50 n)))
                      (inner l m (+ n 1) (+ ret 1))
                      (inner l m (+ n 1) ret)))))
    (print (inner 0 0 0 0))))