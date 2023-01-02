
#lang racket

(require net/http-client)

(provide get-data post-data blockchain-big-bang displayln)

;; get-data : String String -> String
;; Gets the data via HTTPS at the given host and path and returns the text of the response
;; The given host and path should be in the format: "google.com" "/search"
(define (get-data host path)
  (local [(define-values (code headers port) (http-sendrecv host path #:ssl? #t))]
    (read-until-eof port)))

;; post-data : String String String -> Boolean
;; Posts the given data via HTTPS to the given host and path and returns the text of the response
;; The given host and path should be in the format: "google.com" "/search"
(define (post-data host path data)
  (local [(define-values (code headers port)
            (http-sendrecv host path #:ssl? #t #:method #"POST" #:data data))]
    (match (string-split (bytes->string/utf-8 code) " ")
      [`(,version "200" ,response ...) #t]
      [`(,version ,code ,response ...)
       (error (string-append "Failed to send data: "
                             (foldr (λ (string acc) (string-append string " " acc))
                                    (string-append "\n" (read-until-eof port))
                                    response)))])))

(define (read-until-eof port)
  (match (read-line port)
    [(? eof-object?) ""]
    [data (string-append data "\n" (read-until-eof port))]))

;; get-messages : Nat -> [List-of String]
;; Returns a list of messages that have been sent to the broadcaster, starting from the given id.
;; The maximum number of messages returned per function call is 50, therefore if you want to get
;; all the messages you have to call this function multiple times, incrementing the id by 50 each
;; time.
(define (get-messages-str since-id)
  (string-split (get-data "accelchain.api.breq.dev" (string-append "/" (number->string since-id)))
                "\n"))

; String -> [One-of Transaction Block]
(define ((string->message make-transaction make-block) str)
  (local [#;(define _ (displayln str))
          (define split (string-split str ":"))]
    (cond
      [(string=? (second split) "transaction")
       (local [(define move-split (string-split (sixth split) ","))]
         (make-transaction (string->number (first split))
                           (third split)
                           (fourth split)
                           (fifth split)
                           (first move-split)
                           (string->number (second move-split))))]
      [(string=? (second split) "block")
       (make-block
        (map (lambda (str)
               ((string->message make-transaction make-block) (string-replace str ";" ":")))
             (rest (rest (rest (rest split)))))
        (string->number (third split))
        (fourth split))])))

;; message->serial : String -> Nat
(define (message->serial str)
  (local [(define split (string-split str ":"))] (string->number (first split))))

;; make-get-messages : -> ( -> [List-of Message])
;; makes stateful function that returns a list of messages that have been sent
;; to the broadcaster.
(define (make-get-messages make-transaction make-block)
  (local
    [(define last-id 0)
     (define (get-messages-serial serial)
       (local [(define messages-str (get-messages-str serial))
               (define messages (map (string->message make-transaction make-block) messages-str))
               (define next-serial
                 (cond
                   [(empty? messages-str) serial]
                   [else (add1 (message->serial (last messages-str)))]))]
         (list messages next-serial)))]
    (lambda ()
      (local [(define res (get-messages-serial last-id))] (set! last-id (second res)) (first res)))))


(define (ignore-transaction st tr)
  (printf "Ignoring transaction: ~a~n" tr))

(define (ignore-block st tr)
  (printf "Ignoring block: ~a~n" tr))

(define (blockchain-big-bang-f make-transaction
                               make-block
                               transaction?
                               block?
                               init-state
                               on-transaction
                               on-block
                               show-state)
  (let ([get-messages (make-get-messages make-transaction make-block)])
    (let loop ([state init-state] [messages (rest (get-messages))] [empt #false])
      (if (empty? messages)
          (begin
            (display (show-state state))
            (if empt
                state
                (loop state (get-messages) #true))
            )
          (let* ([message (first messages)]
                 [maybe-next-state
                  (with-handlers
                      ([exn:fail? (lambda (e) (printf "Error: ~a~n" e) #f)])
                    (cond
                      [(transaction? message) (on-transaction state message)]
                      [(block? message) (on-block state message)]))])
            (if (not maybe-next-state)
                (begin
                  (printf "Ignoring message: ~a~n" message)
                  (loop state (rest messages) #false))
                (begin
                  (loop maybe-next-state (rest messages) #false))))))))

(define-syntax (blockchain-big-bang stx)
  (syntax-case stx (on-transaction on-block show-state)
    [(_ init-state [on-transaction transaction-handler] [on-block block-handler])
     ;; Has to be copy-pasta because datum->syntax is not the Right Way to do this.
     #`(blockchain-big-bang-f
        #,(datum->syntax stx 'make-transaction)
        #,(datum->syntax stx 'make-block)
        #,(datum->syntax stx 'transaction?)
        #,(datum->syntax stx 'block?)
        init-state
        transaction-handler
        block-handler
        (lambda (s) ""))]
    [(_ init-state [on-transaction transaction-handler] [on-block block-handler] [show-state show-state-handler])
     #`(blockchain-big-bang-f
        #,(datum->syntax stx 'make-transaction)
        #,(datum->syntax stx 'make-block)
        #,(datum->syntax stx 'transaction?)
        #,(datum->syntax stx 'block?)
        init-state
        transaction-handler
        block-handler
        show-state-handler)]))
