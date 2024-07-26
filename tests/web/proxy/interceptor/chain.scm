(use-modules (srfi srfi-64)
             (srfi srfi-26)
             (oop goops)
             (web proxy interceptor chain))


(define %test-name "web/proxy/interceptor/chain.scm")


(test-begin %test-name)

(define %rule-1 '(request method dump stdout))
(define %rule-2 '(response method dump stdout))
(define %rule-3 '(request method replace "POST"))
(define %chain-1 `(,%rule-1 ,%rule-2))

(test-equal "rule:type"
  'request
  (rule:type %rule-1))

(test-equal "rule:field"
  'method
  (rule:field %rule-1))

(test-equal "rule:action"
  'dump
  (rule:action %rule-1))

(test-equal "rule:parameters"
  'stdout
  (rule:parameters %rule-1))

(test-equal "chain-select"
  `(,%rule-1)
  (chain-select %chain-1 'request))

(test-equal "chain-run: dump"
  "\"test\"\n"
  (with-output-to-string
    (lambda ()
      (chain-run (chain-select %chain-1 'request)
                 'method
                 "test"))))

(test-equal "chain-run: replace"
  "\"POST\"\n"
  (with-output-to-string
    (lambda ()
      (let ((chain (chain-select `(,%rule-3 ,%rule-2 ,%rule-1)
                                 'request)))
        (chain-run chain 'method "GET")))))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end %test-name)

(exit (zero? exit-status))
