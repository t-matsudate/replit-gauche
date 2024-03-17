(define-module abc083b
  (export abc083b))
(select-module abc083b)

(define (abc083b)
  (let* ((line (string-split (read-line (current-input-port)) " "))
         (n (string->number (list-ref line 0)))
         (a (string->number (list-ref line 1)))
         (b (string->number (list-ref line 2))))
    (define (inner m ret)
      (if (> m n)
          ret
          (let ((s (reduce + 0 (map digit->integer (string->list (number->string m))))))
            (if (<= a s b)
                (inner (+ m 1) (+ ret m))
                (inner (+ m 1) ret)))))
    (print (inner 1 0))))