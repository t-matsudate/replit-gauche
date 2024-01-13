(define-module abc081a
  (export abc081a))
(select-module abc081a)

(define (abc081a)
  (let* ((line (read-line (current-input-port)))
         (nums (map digit->integer (string->list line))))
    (print (length (filter (lambda (x) (= x 1)) nums)))))