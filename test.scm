#!/usr/bin/env chibi-scheme

(import (chibi) (chibi process) (chibi io))

(define guira (cadr (command-line)))
(define ret 0)
(define red "\x1B;[31m")
(define green "\x1B;[92m")
(define end "\x1B;[0m")

(define (test check-type desc input expect)
  (lambda (pid in out err)
    (write-string input in)
    (close-output-port in)
    (let ((ret-out (port->string-list out))
           (ret-err (port->string err))
           (ret-code (cadr (waitpid pid 0))))
      (if
        (case check-type
          ((1) (= ret-code expect))
          ((2) (equal? ret-out expect)))
        (begin
          (display green)
          (display desc)
          (display ": Passed")
          (display end)
          (newline))
        (begin
          (display
            (string-append
              red desc ": Failed\n"
              "\tstdin: " input
              "\n\tstderr: " ret-err
              "\n\treturn code: " (number->string ret-code)
              end "\n"))
          (set! ret 1))))))

(define tests (list
  (cons
    "-s 1992-04-04 1992-04-06"
    (test 1 "Simple nth day" "(day (nth 3))" 0))

  (cons
    "-s 2000-01-01"
    (test 2 "Complex subselectors"
      "(year 2016
        (month (or jul (nth 8)) (day mon))
        (month dec (day (nth 2))))"
      (list
        "2016-07-04" "2016-07-11" "2016-07-18" "2016-07-25" "2016-08-01"
        "2016-08-08" "2016-08-15" "2016-08-22" "2016-08-29" "2016-12-02")))
  ))

(newline)
(display "|| Starting functional tests ||")
(newline)

(map
  (lambda (args)
    (call-with-process-io
      (string-append guira " " (car args))
      (cdr args)))
  tests)

(newline)
(if (= ret 0)
  (begin
    (display green)
    (display "All functional tests have passed")
    (display end))
  (begin
    (display red)
    (display "Some functional tests have failed")
    (display end)))
(newline)
