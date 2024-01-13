(add-load-path "./" :relative)
(use abc086a)

(define (abc081a)
  (let* ((line (read-line (current-input-port)))
         (nums (map digit->integer (string->list line))))
    (print (length (filter (lambda (x) (= x 1)) nums)))))

(abc086a)
;(abc081a)