#lang racket
;; REQUIRES: raco pkg install crypto
(require crypto)
(require crypto/libcrypto)
(require net/base64)

(provide digest)

;; digest : String -> Nat
;; Produces the SHA256 digest of the given string.
(define (digest str)
  (apply + (map * POWERS (bytes->list (sha256-bytes (string->bytes/utf-8 str))))))

(define POWERS (reverse (build-list 32 (lambda (index) (expt 2 (* 8 index))))))

;; make-secret : -> PrivateKey
;; Generates a new private key. DO NOT SHARE THIS KEY WITH ANYONE!!
(define (make-secret)
  (local [(define rsa-impl (get-pk 'rsa libcrypto-factory))]
    (encode-private-key (generate-private-key rsa-impl '((nbits 512))))))

;; secret->public : PrivateKey -> PublicKey
;; Generates a public key from a private key. You can share this key with anyone, it will be the
;; id for your wallet.
(define (secret->public secret)
  (encode-public-key (pk-key->public-only-key (decode-private-key secret))))

;; make-signature : String PrivateKey -> Signature
;; Signs a string with a private key. You can share the signature with anyone, it will be the
;; cryptographic proof that you signed the string.
(define (make-signature message secret)
  (bytes->string/utf-8
   (base64-encode (digest/sign (decode-private-key secret) 'sha1 message) "")))

;; check-signature : PublicKey String Signature -> Boolean
;; Checks if the given message was signed by the given public key. Returns true if the signature
;; is valid, false otherwise. You want to use this function to check if a given message was
;; made by the given public key.
(define (check-signature id message signature)
  (digest/verify (decode-public-key id) 'sha1 message
                 (base64-decode (string->bytes/utf-8 signature))))

;; unique-string : -> String
;; Creates an unique string for a transaction. this string is guaranteed
;; to be unique among all transactions, as the probability of a collision
;; is 1 in 2^256, which is astronomically small.
(define (unique-string)
  (bytes->string/utf-8 (base64-encode (crypto-random-bytes 32) "")))

;;;;;;;;;;;;;;;; LOW-LEVEL HELPERS, YOU PROBABLY WON'T NEED THESE ;;;;;;;;;;;;;;;;;;;

; encode-private-key : pk-key -> String
; Encodes the internal private key representation of an RSA private key into a string.
(define (encode-private-key key)
  (bytes->string/utf-8 (base64-encode (pk-key->datum key 'RSAPrivateKey) "")))

; decode-private-key :
(define (decode-private-key datum)
  (datum->pk-key (base64-decode (string->bytes/utf-8 datum)) 'RSAPrivateKey libcrypto-factory))

(define (encode-public-key key)
  (list-ref (string-split (pk-key->datum key 'openssh-public) " ") 1))
(define (decode-public-key datum)
  (datum->pk-key (string-append "ssh-rsa " datum) 'openssh-public libcrypto-factory))


(provide
 make-secret
 secret->public
 make-signature
 unique-string
 check-signature)
