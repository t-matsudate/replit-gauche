(define-module abc086a
  (export abc086a))

(select-module abc086a)

(define (abc086a)
  (let* ((line (read-line (current-input-port)))
         (line (string-split line " "))
         (a (string->number (car line)))
         (b (string->number (car (cdr line)))))
    (if (odd? (* a b))
        (print "Odd")
        (print "Even"))))