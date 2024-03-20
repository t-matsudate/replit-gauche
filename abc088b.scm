(define-module abc088b
  (export abc088b))
(select-module abc088b)

(define (abc088b)
  (let ((n (string->number (read-line (current-input-port))))
        (cards (sort (map string->number (string-split (read-line (current-input-port)) " ")) >)))
    (define (inner m cards ret)
      (cond ((or (>= m n) (null? cards)) ret)
            ((even? m) (inner (+ m 1) (cdr cards) (+ ret (car cards))))
            (else (inner (+ m 1) (cdr cards) (- ret (car cards))))))
    (print (inner 0 cards 0))))