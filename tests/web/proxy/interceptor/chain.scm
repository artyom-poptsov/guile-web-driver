(use-modules (srfi srfi-64)
             (srfi srfi-26)
             (oop goops)
             (web proxy interceptor chain))


(define %test-name "web/proxy/interceptor/chain.scm")


(test-begin %test-name)

(define %rule-1 '(request method dump stdout))
(define %rule-2 '(response method dump stdout))
(define %rule-3 '(request method set "POST"))
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

(test-equal "chain-run: set"
  "\"POST\"\n"
  (with-output-to-string
    (lambda ()
      (let ((chain (chain-select `(,%rule-3 ,%rule-2 ,%rule-1)
                                 'request)))
        (chain-run chain 'method "GET")))))

(test-equal "chain-run: replace"
  '((header-1 . "value-1")
    (header-2 . "new-value"))
  (let ((chain '((request headers replace
                          ((header-2 . "new-value"))))))
    (chain-run chain 'headers '((header-1 . "value-1")
                                (header-2 . "value-2")))))

(test-equal "chain-run: append"
  '((header-1 . "value-1")
    (header-2 . "value-2")
    (new-header . "new-value"))
  (let ((chain '((request headers append
                          ((new-header . "new-value"))))))
    (chain-run chain 'headers '((header-1 . "value-1")
                                (header-2 . "value-2")))))

(test-equal "chain-run: delete"
  '((header-1 . "value-1")
    (header-3 . "value-3"))
  (let ((chain '((request headers delete (header-2)))))
    (chain-run chain 'headers '((header-1 . "value-1")
                                (header-2 . "value-2")
                                (header-3 . "value-3")))))

(test-equal "chain-run: custom procedure"
  "\"POST\"\n"
  (with-output-to-string
    (lambda ()
      (let ((chain `((request method ,(lambda (chain prev) (list-ref chain 3))
                              "POST")
                     (request method dump stdout))))
        (chain-run chain 'method "GET")))))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end %test-name)

(exit (zero? exit-status))
