#!/usr/bin/env chibi-scheme

(import (chibi) (chibi process) (chibi io))

(define guira (cadr (command-line)))
(define ret 0)
(define red "\x1B;[31m")
(define green "\x1B;[92m")
(define end "\x1B;[0m")

(define (wexitstatus status)
  (case status
    ((0) 0)
    ((256) 1)
    ((512) 2)
    (else status)))

(define (make-test check-type desc input expect)
  (lambda (pid in out err)
    (write-string input in)
    (close-output-port in)
    (let ((ret-out (port->string-list out))
          (ret-err (port->string err))
          (ret-code (wexitstatus (cadr (waitpid pid 0)))))
      (if
        (case check-type
          ((1) (= ret-code expect))
          ((2) (equal? ret-out expect)))
        (begin
          (display green)
          (display "Passed: ")
          (display desc)
          (display end)
          (newline))
        (begin
          (display
            (string-append
              red "Failed: " desc
              "\n\tstdin: " input
              "\n\tstderr: " ret-err
              "\n\texit code: " (number->string ret-code)
              end "\n"))
          (set! ret 1))))))

(define (test desc type args input expect)
  (cons args (make-test type desc input expect)))

(define tests (list
  (test "Nth day" 1
    "-s 1992-04-04 1992-04-06"
    "(day 3)"
    0)

  (test "Or condition" 2
    "-s 2016-07-03 -e 2016-07-09"
    "(day (or wed fri))"
    (list "2016-07-06" "2016-07-08"))

  (test "Not condition" 1
    "2016-07-04"
    "(day (not mon))"
    1)

  (test "Comments" 1
    "2000-01-01"
    "(day ; comment\n)"
    0)

  (test "Complex subselectors" 2
    "-s 2000-01-01"
    "(year ad2016 (month (or jul 8) (day mon)) (month dec (day 2)))"
    (list
      "2016-07-04" "2016-07-11" "2016-07-18" "2016-07-25" "2016-08-01"
      "2016-08-08" "2016-08-15" "2016-08-22" "2016-08-29" "2016-12-02"))

  (test "First week of month" 2
    "-s 2016-01-01 -i week"
    "(year ad2016 (month sep (week 1)))"
    (list "2016-08-29"))

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
(exit ret)
