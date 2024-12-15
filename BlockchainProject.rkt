;; ACCEL FUNDIES 2
;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hwA_2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require "./crypto-extras.rkt")
(require "./hashtable-extras.rkt")
(require "./http-extras.rkt")

;; A [Hash-Table-of X Y] is a (make-hash (list (list X Y) ...))
;; where X is the key and Y is the value that the key maps to

;; make-hash : [List-of [List-of-Two X Y]] -> [Hash-table-of X Y]
;; Creates a hash table from a list of pairs.

;; hash-has-key? : [Hash-table-of X Y] X -> Boolean
;; Checks if a hash table has a key. Returns #true if it does, #false otherwise.

;; hash-ref : [Hash-table-of X Y] X -> Y
;; Returns the value associated with the key in the hash table.

;; hash-set : [Hash-table-of X Y] X Y -> [Hash-table-of X Y]
;; Returns a new hash table with the key mapped to the value.

;; hash-remove : [Hash-table-of X Y] X -> [Hash-table-of X Y]
;; Returns a new hash table with the key removed

;; hash-keys : [Hash-table-of X Y] -> [List-of X]
;; Returns a list of all the keys in the hash table

;; hash-values : [Hash-table-of X Y] -> [List-of Y]
;; Returns a list of all the values in the hash table

;; ====================================================================================================================

;; A PrivateKey is a String that represents a 512-bit RSA private key.

;; A PublicKey is a String that represents a 512-bit RSA public key.

;; A Signature is a String that represents a 512-bit RSA signature.

;; digest : String -> Nat
;; Produces the SHA256 digest of the given string. SHA256 is a cryptographic
;; hash function that is used in many blockchains, and we will use it too.

;; secret->public : PrivateKey -> PublicKey
;; Generates a public key from a private key.

;; make-signature : String PrivateKey -> Signature
;; Signs a string with a private key.

;; check-signature : PublicKey String Signature -> Boolean
;; Checks if the given string was signed by the given public key.

;; ====================================================================================================================

;; An [Optional X] is one of:
;; - X
;; - #false
;;
;; Interpretation: Either a value of type X or #false

;; ====================================================================================================================

(define-struct transaction [serial unique-string sender-sig sender-key receiver-key amount])
;; A Transaction is a (make-transaction Nat String Signature PublicKey PublicKey Nat)
;;
;; (make-transaction serial unique-string sender-sig sender-key receiver-key amount) represents a
;; single transaction that moves amount accelcoin from sender-key to receiver-key.
;; Moreover:
;;
;; 1. The amount must be positive;
;; 2. The unique-string must be globally unique;
;; 2. The signature signs the string
;;      (string-append unique-string receiver-key ":" (number->string amount))
;;    with the private key corresponding to sender-key.
;; 3. the unique-string is a string that is unique to this transaction.

;; transaction-template : Transaction -> ?
(define (transaction-template tran)
  (... (transaction-serial tran) ...
       (transaction-unique-string tran) ...
       (transaction-sender-sig tran) ...
       (transaction-sender-key tran) ...
       (transaction-receiver-key tran) ...
       (transaction-amount tran) ...))

(define-struct block [transactions nonce miner-key])
;; A Block is a (make-block [List-of Transaction] Nat PublicKey)
;;
;; (make-block transactions nonce miner-key) represents a block of transactions mined by miner-key.
;; The transactions are processed left-to-right. Thus (first transactions) occurs before
;; (second transactions).

;; block-template : Block -> ?
(define (block-template b)
  (... (block-transactions b) ... (block-nonce b) ... (block-miner-key b) ...))

;; A Blockchain is a [NE-List-of Block]
;;
;; The first element of a Blockchain is the latest block and the last element is the first
;; block or the *genesis block*. The genesis block has zero transactions and all other blocks have
;; three or more transactions.
(define (blockchain-template bchain)
  (cond
    [(empty? (rest bchain)) ...]
    [(cons? bchain) (... (first bchain) ... (blockchain-template (rest bchain)) ...)]))

;; ====================================================================================================================

(define ALICE-PRIVATE-KEY
  "MIIBOgIBAAJBAMrPOfefdvowOwAplxY/NLkJFymyedikvwvsyhtQ98CawNXeKydg+WYD9YzQW1tIY5Ta1bqZhk5hp
WGM4eusKxkCAwEAAQJAMtQW2hmsLu3xi4vg4uF6bDl8BaZGZWZ8vxdcW9ZCEZIEtnYGlkpwoG5YcUp3a39YRP+Nt2fA6bmPbvjmWAspkQIhAPodYjlh0p
7P4QodsvQinMRp9Z8knfBmYeQpg/0otBMVAiEAz5Tjyw0Yanh6CCIvDRKQ+SvdTMvrJykIMyzmsWgYSPUCIEwGvIG2w3/0rnIVvvzIvKBTmQ7L4ZpedK
kXGYDNa5dVAiAfRL5Lh911rFA1iXCs927/GaxsNQtnCrdBfjIB5zxBQQIhAO0ZN+PGdjJfbhivUdgfx+DbrHkClSWT8SidILAbgQkd")
(define BOB-PRIVATE-KEY
  "MIIBOwIBAAJBAKy4zO2w1HfXMNHSCYKuheD+5ZkAlHubePYNOVvi3gA/AQ1S0HcRFmTkzFz/SCp+0cZ3wErzHhKXmvg
IrjLbdYMCAwEAAQJACBwBGyPTRfEnjKJk6erRxFeTZhSd5BPPoRXL3KGRNMesv5qct9QNbHA2ghjY4Z1gokwLgCViG88FvG0qMKGNSQIhANduvtUGG
vqeb+c6khwi60sf/3KMa082IjC3fe4RosJPAiEAzT8eusKDsL3q38i1o6E4pzUuW4oK0ta1BCGEdZn2kI0CIDb6bz8ECNyOlHZJL0J48t1ANDuydCx
J313ZZgzceVHnAiEApVA7vg1B6K9vaIPO2VbXvMW26wAKq7tH3WXpvJcf41kCIQCTv8zWOp8Dq3NKTdFZD28NCohpiEOAP3yMng9HhXcAqg==")
(define ALICE-PUBLIC-KEY (secret->public ALICE-PRIVATE-KEY))
(define BOB-PUBLIC-KEY (secret->public BOB-PRIVATE-KEY))

(define MY-SECRET-KEY
  (string-append "MIIBPQIBAAJBALtU5qAWHMHgZz9YL7o1AZaga9rOlPCCqdYOLPprcZkRwB4ncV6RuUJAYV6fAdI690o"
                 "3GwNXmQYt1y4KlgkxTUECAwEAAQJBAKMganfq6X3WmHHrrBv9PnXZGjJ+g+nwDk7NJ1OlrlU4RHPO/f"
                 "In8OcZEkRRbmNJCY6Z/ttRP2/P3S9vtAN8PTUCIQD4EG6xkVvZ/jz+jnPk1R6evcePA22QQjrpFKiyQ"
                 "8sQ7wIhAMFTFos7XBYdBK9rfGyTtwoSN7pgfjJe9XXHnHxWMyTPAiEA9gM99GYnRLtJ/F6Ee2YM1vt5h"
                 "NCugQnd6LgvUZPAOQsCIQCPV50ilVvdJZ0AuJDkLM6OEB3z+nH3xUL5Fo1n/74iLwIhAM2ESCr8fTSae"
                 "iZFjl3Ff51BvWFIw/B50kZ6qSvy/qR3"))

(define MY-PUBLIC-KEY (secret->public MY-SECRET-KEY))

;; build-transaction: Nat ( -> String) PrivateKey PublicKey Nat -> Transaction
;; (build-transaction serial unique-string sender-private-key receiver-public-key amount) builds a transaction
;; that sends amount from the sender to the receiver.
(define (build-transaction serial unique-string sender-private-key receiver-public-key amount)
  (make-transaction
   serial
   unique-string
   (make-signature (string-append unique-string receiver-public-key ":" (number->string amount))
                   sender-private-key)
   (secret->public sender-private-key)
   receiver-public-key
   amount))

;; Sends 100 accelcoins from Alice to Bob
(define EX-TRANSACTION-0
  (make-transaction
   0
   "unique"
   (make-signature (string-append "unique" ALICE-PUBLIC-KEY ":" (number->string 100)) BOB-PRIVATE-KEY)
   BOB-PRIVATE-KEY
   ALICE-PUBLIC-KEY
   100))

;; transaction->string : Transaction -> String
;; Serializes a transaction into a string with the format
;; "serial:transaction:unique-string:sender-sig:sender-key:receiver-key,amount"

(define (transaction->string t)
  (string-append (number->string (transaction-serial t))
                 ":"
                 "transaction:"
                 (transaction-unique-string t)
                 ":"
                 (transaction-sender-sig t)
                 ":"
                 (transaction-sender-key t)
                 ":"
                 (transaction-receiver-key t)
                 ","
                 (number->string (transaction-amount t))))

(check-expect
 (transaction->string EX-TRANSACTION-0)
 (string-append
  "0:transaction:unique:L91Sd73xDN2EqA+jdbP5TeVqB9STwsVY/yUA1FMDbrt6j9Aqvmc49siBnk7G365R32rh"
  "ZYaZwkJkYOKN+ojVqg==:MIIBOwIBAAJBAKy4zO2w1HfXMNHSCYKuheD+5ZkAlHubePYNOVvi3gA/AQ1S0HcRFmTk"
  "zFz/SCp+0cZ3wErzHhKXmvg\nIrjLbdYMCAwEAAQJACBwBGyPTRfEnjKJk6erRxFeTZhSd5BPPoRXL3KGRNMesv5"
  "qct9QNbHA2ghjY4Z1gokwLgCViG88FvG0qMKGNSQIhANduvtUGG\nvqeb+c6khwi60sf/3KMa082IjC3fe4RosJP"
  "AiEAzT8eusKDsL3q38i1o6E4pzUuW4oK0ta1BCGEdZn2kI0CIDb6bz8ECNyOlHZJL0J48t1ANDuydCx\nJ313ZZg"
  "zceVHnAiEApVA7vg1B6K9vaIPO2VbXvMW26wAKq7tH3WXpvJcf41kCIQCTv8zWOp8Dq3NKTdFZD28NCohpiEOAP3"
  "yMng9HhXcAqg==:AAAAB3NzaC1yc2EAAAADAQABAAAAQQDKzzn3n3b6MDsAKZcWPzS5CRcpsnnYpL8L7MobUPfAm"
  "sDV3isnYPlmA/WM0FtbSGOU2tW6mYZOYaVhjOHrrCsZ,100"))

;; Sends 150 accelcoins from Alice to Bob
(define EX-TRANSACTION-1 (build-transaction 1 (unique-string) ALICE-PRIVATE-KEY BOB-PUBLIC-KEY 15))

;; Sends 200 accelcoins from Alice to Bob
(define EX-TRANSACTION-2 (build-transaction 2 (unique-string) ALICE-PRIVATE-KEY BOB-PUBLIC-KEY 20))

;; Sends 300 accelcoins from Alice to Bob
(define EX-TRANSACTION-3 (build-transaction 3 (unique-string) ALICE-PRIVATE-KEY BOB-PUBLIC-KEY 30))

;; Sends 1000 accelcoins from Alice to Bob
(define EX-TRANSACTION-4 (build-transaction 4 (unique-string) BOB-PRIVATE-KEY ALICE-PUBLIC-KEY 90))

;; Sends 150 accelcoins from Bob to Alice
(define EX-TRANSACTION-5 (build-transaction 5 (unique-string) BOB-PRIVATE-KEY ALICE-PUBLIC-KEY 15))

;; Sends 200 accelcoins from Bob to Alice
(define EX-TRANSACTION-6 (build-transaction 6 (unique-string) BOB-PRIVATE-KEY ALICE-PUBLIC-KEY 20))

;; Sends 300 accelcoins from Bob to Alice
(define EX-TRANSACTION-7 (build-transaction 7 (unique-string) BOB-PRIVATE-KEY ALICE-PUBLIC-KEY 30))

;; Sends 1000 accelcoins from Bob to Alice
(define EX-TRANSACTION-8 (build-transaction 8 (unique-string) BOB-PRIVATE-KEY ALICE-PUBLIC-KEY 10))

;; Bad Transactions
(define BAD-TRANSACTION-1
  (make-transaction 20
                    (unique-string)
                    "get hacked"
                    (secret->public ALICE-PRIVATE-KEY)
                    BOB-PUBLIC-KEY
                    102020210))
(define BAD-TRANSACTION-2
  (make-transaction 21
                    (unique-string)
                    "get hacked2"
                    (secret->public BOB-PRIVATE-KEY)
                    ALICE-PUBLIC-KEY
                    12983))
(define BAD-TRANSACTION-3
  (make-transaction 22
                    (unique-string)
                    "get hacked3"
                    (secret->public BOB-PRIVATE-KEY)
                    ALICE-PUBLIC-KEY
                    124892734))

;; A genesis block where Alice starts the blockchain and receives the first mining reward.
(define EX-BLOCK-0
  (make-block '()
              8631727707325622792404128232286945630015639849891523695238049493932286431978
              ALICE-PUBLIC-KEY))

;;Hand-made example blocks
(define REAL-BLOCK-1
  (make-block (list EX-TRANSACTION-3 EX-TRANSACTION-2 EX-TRANSACTION-4) 0 BOB-PUBLIC-KEY))
(define REAL-BLOCK-2
  (make-block (list EX-TRANSACTION-5 EX-TRANSACTION-6 EX-TRANSACTION-7) 0 ALICE-PUBLIC-KEY))
(define REAL-BLOCK-3
  (make-block (list EX-TRANSACTION-4 EX-TRANSACTION-1 EX-TRANSACTION-2) 0 BOB-PUBLIC-KEY))

;;Blocks with invalid signatures
(define SCAM-BLOCK-1
  (make-block (list BAD-TRANSACTION-1 BAD-TRANSACTION-2 BAD-TRANSACTION-3) 0 BOB-PUBLIC-KEY))

;;Example blockchains
(define REAL-BLOCKCHAIN-1 (list REAL-BLOCK-1 REAL-BLOCK-2))
(define REAL-BLOCKCHAIN-2 (list REAL-BLOCK-2 REAL-BLOCK-3))
(define REAL-BLOCKCHAIN-3 (list REAL-BLOCK-1 REAL-BLOCK-2 REAL-BLOCK-3))

;; block-digest: Digest Block -> Digest
;; (block-digest prev-digest block) computes the digest of block, given the digest
;; of the previous block.
;;
;; The digest must be the digest of the following strings concatenated in order:
;;
;; 1. prev-digest as a string
;; 2. The transactions as strings (using transaction->string) concatenated in order
;; 3. The nonce as a string

(define (block-digest prev-digest block)
  (digest (foldr string-append
                 ""
                 (append (list (number->string prev-digest))
                         (map (λ (x) (transaction->string x)) (block-transactions block))
                         (list (number->string (block-nonce block)))))))

;; Copy this definition to your solution
(define DIGEST-LIMIT (expt 2 (* 8 30)))

;; mine-block : Digest PublicKey [List-of Transaction] Nat -> [Optional Block]
;; (mine-block prev-digest miner-public-key transactions trials)
;; tries to mine a block, but gives up after trials attempts.
;; The produced block has a digest that is less than DIGEST-LIMIT.

(define (mine-block prev-digest miner-public-key transactions trials)
  (local ;;guess is a guess for the nonce block
   [(define guess (random 4294967087))]
   (cond
     [(>= 0 trials) #false]
     [(<= DIGEST-LIMIT (block-digest prev-digest (make-block transactions guess miner-public-key)))
      (mine-block prev-digest miner-public-key transactions (- trials 1))]
     [else (make-block transactions guess miner-public-key)])))

;;Example mined blocks
(define REALER-BLOCK-1 (mine-block 0 BOB-PUBLIC-KEY '() 1000000))
(define REALER-BLOCK-2
  (mine-block (block-digest 0 REALER-BLOCK-1)
              ALICE-PUBLIC-KEY
              (list EX-TRANSACTION-4 EX-TRANSACTION-5 EX-TRANSACTION-6)
              1000000))
(define REALER-BLOCK-3
  (mine-block (block-digest (block-digest 0 REALER-BLOCK-1) REALER-BLOCK-2)
              BOB-PUBLIC-KEY
              (list EX-TRANSACTION-1 EX-TRANSACTION-4 EX-TRANSACTION-7)
              1000000))
(define BAD-BLOCK-1
  (mine-block 20 BOB-PUBLIC-KEY (list EX-TRANSACTION-1 EX-TRANSACTION-4 EX-TRANSACTION-7) 1))
(define BAD-BLOCK-2
  (mine-block 0 ALICE-PUBLIC-KEY (list EX-TRANSACTION-4 EX-TRANSACTION-5 EX-TRANSACTION-6) 1))
(define BAD-BLOCK-3
  (mine-block 0 ALICE-PUBLIC-KEY (list EX-TRANSACTION-1 EX-TRANSACTION-2 EX-TRANSACTION-3) 1))

;;Example mined blockchains
(define REALER-BLOCKCHAIN-1 (list REALER-BLOCK-2 REALER-BLOCK-1))
(define REALER-BLOCKCHAIN-2 (list REALER-BLOCK-3 REALER-BLOCK-2))
(define REALER-BLOCKCHAIN-3 (list REALER-BLOCK-3 REALER-BLOCK-2 REALER-BLOCK-1))
(define BAD-BLOCKCHAIN-1 (list BAD-BLOCK-1 BAD-BLOCK-2))
(define BAD-BLOCKCHAIN-2 (list BAD-BLOCK-2 BAD-BLOCK-3))
(define BAD-BLOCKCHAIN-3 (list BAD-BLOCK-1 BAD-BLOCK-2 BAD-BLOCK-3))

;; blocks-sized-ok? : Blockchain -> Boolean
;; Determines that every block has at least three transactions, and that
;; the genesis block has zero transactions.

(define (blocks-sized-ok? bchain)
  (cond
    [(empty? (rest bchain)) (= 0 (length (block-transactions (first bchain))))]
    [else
     (if (<= 3 (first (map (λ (x) (length (block-transactions x))) bchain)))
         (blocks-sized-ok? (rest bchain))
         #false)]))

(define ERROR-BLOCK-1 (make-block (list EX-TRANSACTION-3 EX-TRANSACTION-2) 0 BOB-PUBLIC-KEY))
(define ERROR-BLOCK-2 (make-block (list EX-TRANSACTION-2) 0 BOB-PUBLIC-KEY))

(check-expect (blocks-sized-ok? (list ERROR-BLOCK-1 EX-BLOCK-0)) #false)
(check-expect (blocks-sized-ok? (list ERROR-BLOCK-2)) #false)
(check-expect (blocks-sized-ok? (list EX-BLOCK-0)) #true)
(check-expect (blocks-sized-ok? (list REAL-BLOCK-1 EX-BLOCK-0)) #true)
(check-expect (blocks-sized-ok? (list REAL-BLOCK-2 REAL-BLOCK-3 EX-BLOCK-0)) #true)

;; no-duplicate-transactions? : Blockchain -> Boolean
;; Determines if every transaction in the blockchain appears exactly once. Every
;; transaction has a unique serial number that we use to determine if it is unique.

(define (no-duplicate-transactions? bchain)
  (andmap (λ (x) (<= (length x) 1))
          (group-by =
                    (sort (map (λ (y) (transaction-serial y))
                               (foldr append '() (map (λ (x) (block-transactions x)) bchain)))
                          <))))

(check-expect (no-duplicate-transactions? (list REAL-BLOCK-2 EX-BLOCK-0)) #true)
(check-expect (no-duplicate-transactions? (list REAL-BLOCK-1 REAL-BLOCK-2 REAL-BLOCK-3 EX-BLOCK-0))
              #false)
(check-expect (no-duplicate-transactions? (list REAL-BLOCK-1 REAL-BLOCK-2 EX-BLOCK-0)) #true)

;; group-by : (X X -> Boolean) [List-of X] -> [List-of [NE-List-of X]]
;; (group-by same-group? alist) splits alist into groups by comparing
;; consecutive elements to check if they should be in the same group. (PASTED FROM PREVIOUS HOMEWORK TO USE AS HELPER)
(define (group-by same-group? lox)
  (local
   ;; prev : X
   ;; curr : [X]
   ;; lolox : [[X]]
   ((define-struct acc [prev curr lolox])
    (define (help x acc)
      (if (empty? (acc-curr acc))
          (make-acc x (list x) (acc-lolox acc))
          (if (same-group? x (acc-prev acc))
              (make-acc x (cons x (acc-curr acc)) (acc-lolox acc))
              (make-acc x (list x) (cons (acc-curr acc) (acc-lolox acc))))))
    (define folded (foldr help (make-acc #f null null) lox)))
   (if (empty? (acc-curr folded)) (acc-lolox folded) (cons (acc-curr folded) (acc-lolox folded)))))

;; all-signatures-ok? : Blockchain -> Boolean
;; Determines if every transaction in the blockchain has a valid signature.
(define (all-signatures-ok? bchain)
  (andmap (λ (x)
            (check-signature (transaction-sender-key x)
                             (string-append (transaction-unique-string x)
                                            (transaction-receiver-key x)
                                            ":"
                                            (number->string (transaction-amount x)))
                             (transaction-sender-sig x)))
          (foldr append '() (map (λ (x) (block-transactions x)) bchain))))

(check-expect (all-signatures-ok? (list SCAM-BLOCK-1)) #false)
(check-expect (all-signatures-ok? (list REAL-BLOCK-1 SCAM-BLOCK-1)) #false)
(check-expect (all-signatures-ok? (list REAL-BLOCK-1 REAL-BLOCK-2)) #true)

;; valid-digests? : Blockchain -> Boolean
;; Determines if every block has a valid digest.
;; valid-digests? : Blockchain -> Boolean
;; Determines if every block has a valid digest.
(define (valid-digests? bchain)
  (local
   [;; valid-digests-helper : Digest Blockchain -> Boolean
    ;; (valid-digests-helper bc) produces #false if any block has an invalid digest.
    (define (valid-digests-helper pd bchain)
      (cond
        [(or (empty? (rest bchain)) (empty? bchain)) #true]
        [else
         (if (>= DIGEST-LIMIT (block-digest pd (second (reverse bchain))))
             (valid-digests-helper (block-digest pd (second (reverse bchain)))
                                   (reverse (rest (reverse bchain))))
             #false)]))]
   (cond
     [(and (empty? (block-transactions (first (reverse bchain)))) (empty? (rest bchain))) #false]
     [(empty? (block-transactions (first (reverse bchain))))
      (if (< (block-digest 0 (second (reverse bchain))) DIGEST-LIMIT)
          #false
          (valid-digests-helper (block-digest 0 (second (reverse bchain)))
                                (reverse (rest (reverse bchain)))))])))

(check-expect (valid-digests? REALER-BLOCKCHAIN-1) #true)
(check-expect (valid-digests? REALER-BLOCKCHAIN-3) #false)

;; hash-update : [Hash-table-of X Y] X (Y -> Y) Y
;; updates entry using function if present, else default
(define (hash-update h k upd def)
  (hash-set h k (if (hash-has-key? h k) (upd (hash-ref h k)) def)))

(check-expect (hash-update (make-hash (list)) "foo" add1 0) (make-hash (list (list "foo" 0))))
(check-expect (hash-update (make-hash (list (list "foo" 0) (list "bar" 0))) "foo" add1 0)
              (make-hash (list (list "foo" 1) (list "bar" 0))))

;; A Ledger is a [Hash-Table-of PublicKey Nat]
;; A ledger maps wallet IDs (public keys) to the number of accelcoins they have.

;; reward : PublicKey Ledger -> Ledger
;; Grants the miner the reward for mining a block.
(define (reward PubKey Ledger)
  (hash-update Ledger PubKey (lambda (x) (+ 100 x)) 100))

(check-expect (reward BOB-PUBLIC-KEY (make-hash (list (list BOB-PUBLIC-KEY 200))))
              (make-hash (list (list BOB-PUBLIC-KEY 300))))




;; update-ledger/transaction: Transaction Ledger -> [Optional Ledger]
;; Updates the ledger with a single transaction. Produces #false if
;; the sender does not have enough accelcoin to send.
(define (update-ledger/transaction transact Ledger)
  (if (hash-has-key? Ledger (transaction-sender-key transact))
      (cond
        [(> (transaction-amount transact) (hash-ref Ledger (transaction-sender-key transact))) #false]
        [else
         (hash-update (hash-set Ledger
                                (transaction-sender-key transact)
                                (- (hash-ref Ledger (transaction-sender-key transact))
                                   (transaction-amount transact)))
                      (transaction-receiver-key transact)
                      (lambda (x) (+ x (transaction-amount transact)))
                      (transaction-amount transact))])
      #false))

(check-expect (update-ledger/transaction EX-TRANSACTION-6 (make-hash '())) #false)

(check-expect
 (update-ledger/transaction EX-TRANSACTION-6 (make-hash (list (list BOB-PUBLIC-KEY 100))))
 (make-hash
  (list (list (string-append
               "AAAAB3NzaC1yc2EAAAADAQABAAAAQQDKzzn3n3b6MDsAKZcWPzS5CRcpsnnYpL8L7MobUPfAmsDV3i"
               "snYPlmA/WM0FtbSGOU2tW6mYZOYaVhjOHrrCsZ")
              20)
        (list (string-append
               "AAAAB3NzaC1yc2EAAAADAQABAAAAQQCsuMztsNR31zDR0gmCroXg/uWZAJR7m3j2DTlb4t4APwENUtB3ER"
               "Zk5Mxc/0gqftHGd8BK8x4Sl5r4CK4y23WD")
              80))))


;; update-ledger/block : Block Ledger -> [Optional Ledger]
;; Updates the ledger with the transactions in a block, and rewards the miner.
;; Produces #false if any transaction in the block would make a sender's
;; balance negative. The miner receives their reward *after* all transactions
;; are procsed.
(define (update-ledger/block block Ledger)
  (cond
    [(boolean? Ledger) #false]
    [(empty? (block-transactions block)) (reward (block-miner-key block) Ledger)]
    [(cons? (block-transactions block))
     (update-ledger/block
      (make-block (rest (block-transactions block)) (block-nonce block) (block-miner-key block))
      (update-ledger/transaction (first (block-transactions block)) Ledger))]))
#|
(check-expect (update-ledger/block REALER-BLOCK-1 (make-hash '()))
              (make-hash (list (list (string-append "AAAAB3NzaC1yc2EAAAADAQABAAAAQQCs"
                                                    "uMztsNR31zDR0gmCroXg/uWZAJR7m3j2"
                                                    "DTlb4t4APwENUtB3ERZk5Mxc/0gqftHG"
                                                    "d8BK8x4Sl5r4CK4y23WD")
                                     100))))
|#

(check-expect
 (update-ledger/block REALER-BLOCK-2
                      (make-hash (list (list BOB-PUBLIC-KEY 100000) (list ALICE-PUBLIC-KEY 10000))))
 (make-hash
  (list (list (string-append
               "AAAAB3NzaC1yc2EAAAADAQABAAAAQQDKzzn3n3b6MDsAKZcWPzS5CRcpsnnYpL8L7MobUPfAmsDV3isnYP"
               "lmA/WM0FtbSGOU2tW6mYZOYaVhjOHrrCsZ")
              10225)
        (list (string-append
               "AAAAB3NzaC1yc2EAAAADAQABAAAAQQCsuMztsNR31zDR0gmCroXg/uWZAJR7m3j2DTlb4t4APwENUtB3ERZ"
               "k5Mxc/0gqftHGd8BK8x4Sl5r4CK4y23WD")
              99875))))

;; update-ledger/blockchain : Blockchain Ledger -> [Optional Ledger]
;; Produces the ledger for a blockchain, or #false if any transaction is invalid
(define (update-ledger/blockchain blockchain Ledger)
  (cond
    [(boolean? Ledger) #false]
    [(empty? blockchain) Ledger]
    [else
     (update-ledger/blockchain (rest blockchain) (update-ledger/block (first blockchain) Ledger))]))

(check-expect (update-ledger/blockchain REALER-BLOCKCHAIN-1 (make-hash '())) #false)

(define (account-balances-ok? bc)
  (not (false? (update-ledger/blockchain bc (make-hash '())))))

(check-expect (account-balances-ok? '()) #true)
(check-expect (account-balances-ok? (list REALER-BLOCK-1)) #true)
(check-expect (account-balances-ok? REALER-BLOCKCHAIN-2) #false)

;; valid-blockchain? : Blockchain -> Boolean
;; Determines if a blockchain is valid.
(define (valid-blockchain? bc)
  (and (blocks-sized-ok? bc)
       (no-duplicate-transactions? bc)
       (all-signatures-ok? bc)
       (valid-digests? bc)
       (account-balances-ok? bc)))

;; send-transaction: PrivateKey PublicKey Nat -> Boolean
;; (send-transaction sender-private-key receiver-public-key amount) sends a
;; transactions to the Accelchain broadcaster.
(define (send-transaction sender-private-key receiver-public-key amount)
  (local [;; local variable holding a unique string
          (define us (unique-string))]
         (post-data "accelchain.api.breq.dev"
                    "/"
                    (string-append "transaction:"
                                   us
                                   ":"
                                   (make-signature
                                    (string-append us receiver-public-key ":" (number->string amount))
                                    sender-private-key)
                                   ":"
                                   (secret->public sender-private-key)
                                   ":"
                                   receiver-public-key
                                   ","
                                   (number->string amount)))))

(check-expect (send-transaction
               (string-append "MIIBPQIBAAJBALtU5qAWHMHgZz9YL7o1AZaga9rOlPCCqdYOLPprcZkRwB4ncV6RuUJAY"
                              "V6fAdI690o3GwNXmQYt1y4KlgkxTUECAwEAAQJBAKMganfq6X3WmHHrrBv9PnXZGjJ+g+"
                              "nwDk7NJ1OlrlU4RHPO/fIn8OcZEkRRbmNJCY6Z/ttRP2/P3S9vtAN8PTUCIQD4EG6xkVv"
                              "Z/jz+jnPk1R6evcePA22QQjrpFKiyQ8sQ7wIhAMFTFos7XBYdBK9rfGyTtwoSN7pgfjJe"
                              "9XXHnHxWMyTPAiEA9gM99GYnRLtJ/F6Ee2YM1vt5hNCugQnd6LgvUZPAOQsCIQCPV50il"
                              "VvdJZ0AuJDkLM6OEB3z+nH3xUL5Fo1n/74iLwIhAM2ESCr8fTSaeiZFjl3Ff51BvWFIw/"
                              "B50kZ6qSvy/qR3")
               (string-append "AAAAB3NzaC1yc2EAAAADAQABAAAAQQC7VOagFhzB4Gc/WC+6NQGWoGvazpTwgqnWDiz6a"
                              "3GZEcAeJ3FekblCQGFenwHSOvdKNxsDV5kGLdcuCpYJMU1B")
               1)
              #true)

;; An OldValidatorState is a (make-old-validator-state ledger blockchain [List-of Transactions])
;; It stores the ledger, blockchain, and unprocessed transactions to be used for validation.
(define-struct old-validator-state [ledger blockchain unproc-transactions])

;;Example Old Validator States
(define v1
  (make-old-validator-state
   (make-hash (list (list (string-append
                           "AAAAB3NzaC1yc2EAAAADAQABAAAAQQDbXz4rfbrRrXYQJbwuCkIy"
                           "IsccHRpxhxqxgKeneVF4eUXof6e2nLvdXkGA0Y6uBAQ6N7qKxasVTR/2s1N2OBWF")
                          100)))
   (list (make-block '()
                     1337
                     (string-append "AAAAB3NzaC1yc2EAAAADAQABAAAAQQDbXz4rfbrRrXYQJbwuCkIyIsccHR"
                                    "pxhxqxgKeneVF4eUXof6e2nLvdXkGA0Y6uBAQ6N7qKxasVTR/2s1N2OBWF")))
   '()))
(define v2
  (make-old-validator-state
   (make-hash (list (list BOB-PUBLIC-KEY 1000000000000) (list ALICE-PUBLIC-KEY 10000000)))
   (list REALER-BLOCK-1)
   '()))
;; Some more example transactions and blocks
(define REP-US-1 (build-transaction 30 "hi" ALICE-PRIVATE-KEY BOB-PUBLIC-KEY 16))
(define REP-US-2 (build-transaction 40 "hi" ALICE-PRIVATE-KEY BOB-PUBLIC-KEY 17))

(define BAD-BLOCK-4
  (mine-block 20 BOB-PUBLIC-KEY (list REP-US-2 EX-TRANSACTION-1 EX-TRANSACTION-2) 1000000000000000))

;; One more example validator state
(define v3
  (make-old-validator-state
   (make-hash (list (list BOB-PUBLIC-KEY 1000000000000) (list ALICE-PUBLIC-KEY 10000000)))
   (list BAD-BLOCK-4)
   '()))

;; NEW HW A ValidatorStates
;; A ValidatorState is a
;; (make-validator-state ledger [HashTable-of Nat Transaction] [HashTable-of String Boolean] String)
;; It stores the ledger, unprocessed transactions, unique-strings, and the last digest to be used for validation.
(define-struct validator-state [ledger unproc-transactions unique-strings last-digest])

(define nv1
  (make-validator-state
   (make-hash (list (list (string-append
                           "AAAAB3NzaC1yc2EAAAADAQABAAAAQQDbXz4rfbrRrXYQJbwuCkIy"
                           "IsccHRpxhxqxgKeneVF4eUXof6e2nLvdXkGA0Y6uBAQ6N7qKxasVTR/2s1N2OBWF")
                          100)))
   (make-hash (list (list '() '())))
   (make-hash (list (list '() '())))
   ""))

(define nv2
  (make-validator-state
   (make-hash (list (list BOB-PUBLIC-KEY 1000000000000) (list ALICE-PUBLIC-KEY 10000000)))
   (make-hash (list (list '() '())))
   (make-hash (list (list '() '())))
   ""))

(define nv3
  (make-validator-state
   (make-hash (list (list BOB-PUBLIC-KEY 1000000000000) (list ALICE-PUBLIC-KEY 10000000)))
   (make-hash (list (list '() '())))
   (make-hash (list (list '() '())))
   ""))

(define empty-vs
  (make-validator-state
   (make-hash (list (list '() '())))
   (make-hash (list (list '() '())))
   (make-hash (list (list '() '())))
   ""))

(define start-state
  (make-validator-state
   (make-hash (list (list (string-append
                           "AAAAB3NzaC1yc2EAAAADAQABAAAAQQDbXz4rfbrRrXYQJbwuCkIy"
                           "IsccHRpxhxqxgKeneVF4eUXof6e2nLvdXkGA0Y6uBAQ6N7qKxasVTR/2s1N2OBWF")
                          100)))
   (make-hash (list (list '() '())))
   (make-hash (list (list '() '())))
   (block-digest 0 (make-block '() 1337 (string-append "AAAAB3NzaC1yc2EAAAADAQABAAAAQQDbXz4rfbrRrXYQJbwuCkIyIsccHRpxhx"
                                                       "qxgKeneVF4eUXof6e2nLvdXkGA0Y6uBAQ6N7qKxasVTR/2s1N2OBWF")))))
  


;; old-validator-state-template : OldValidatorState -> ?
;; Template for accessing OldValidatorStates.
(define (old-validator-state-template vs)
  (... (old-validator-state-ledger vs) ...
       (old-validator-state-blockchain vs) ...
       (old-validator-state-unproc-transactions vs)))

;; validator-state-template : ValidatorState -> ?
;; Template for accessing ValidatorStates
(define (validator-state-template vs)
  (... (validator-state-ledger vs) ...
       (validator-state-unproc-transactions vs) ...
       (validator-state-unique-strings vs) ...
       (validator-state-last-digest vs)))


;; handle-transaction : ValidatorState Transaction -> [Optional ValidatorState]
;; Updates the ValidatorState with the unprocessed transaction if
;; signature, key, and actual transaction are all valid.
(define (handle-transaction vs tr)
  (cond
    [(false? (update-ledger/transaction tr (validator-state-ledger vs))) #false]
    [(false? (check-signature (transaction-sender-key tr)
                              (string-append (transaction-unique-string tr)
                                             (transaction-receiver-key tr)
                                             ":"
                                             (number->string (transaction-amount tr)))
                              (transaction-sender-sig tr)))
     #false]
    [else
     (make-validator-state (validator-state-ledger vs)
                           (hash-set (validator-state-unproc-transactions vs) (transaction-serial tr) tr)
                           (hash-set (validator-state-unique-strings vs) (transaction-unique-string tr) #true)
                           (validator-state-last-digest vs))]))

;;(check-expect (handle-transaction v3 REP-US-1) #false)
;;(check-expect (handle-transaction v2 BAD-TRANSACTION-1) #false)
;;(check-expect (handle-transaction v2 BAD-TRANSACTION-2) #false)
#|(check-expect
 (handle-transaction v2 EX-TRANSACTION-1)
 (make-old-validator-state
  (make-hash
   (list
    (list (string-append "AAAAB3NzaC1yc2EAAAADAQABAAAAQQDKzzn3n3b6MDsAKZcWPzS5CRcpsnnYpL8L7MobUP"
                         "fAmsDV3isnYPlmA/WM0FtbSGOU2tW6mYZOYaVhjOHrrCsZ")
          10000000)
    (list (string-append "AAAAB3NzaC1yc2EAAAADAQABAAAAQQCsuMztsNR31zDR0gmCroXg/uWZAJR7m3j2DTlb4t4"
                         "APwENUtB3ERZk5Mxc/0gqftHGd8BK8x4Sl5r4CK4y23WD")
          1000000000000)))
  (list REALER-BLOCK-1)
  (list EX-TRANSACTION-1)))
|#
;; handle-block : ValidatorState Block -> [Optional ValidatorState]
;; Updates the ValidatorState with the block added to the blockchain if
;; transactions and the block digests are valid.
(define (handle-block vs block)
  (cond
    [(not (ormap (lambda (x) (member x (remove x (block-transactions block)))) (block-transactions block))) #false]
    [(member #false (map (lambda (x) (handle-transaction vs x)) (block-transactions block))) #false]
    [(< (length (block-transactions block)) 3) #false]
    [(false? (update-ledger/block block (validator-state-ledger vs))) #false]
    [else
     (make-validator-state (update-ledger/block block (validator-state-ledger vs))
                           (validator-state-unproc-transactions vs)
                           (validator-state-unique-strings vs)
                           (block-digest (validator-state-last-digest vs) block))]))

(check-expect (handle-block v1
                            (mine-block (block-digest (block-digest 0 REALER-BLOCK-1) REALER-BLOCK-2)
                                        BOB-PUBLIC-KEY
                                        (list EX-TRANSACTION-1 EX-TRANSACTION-4)
                                        1000000))
              #false)
(check-expect (handle-block v1 REALER-BLOCK-1) #false)
(check-expect (handle-block v1 REALER-BLOCK-3) #false)
(check-expect (handle-block v2
                            (mine-block (block-digest (block-digest 0 REALER-BLOCK-1) REALER-BLOCK-2)
                                        BOB-PUBLIC-KEY
                                        (list EX-TRANSACTION-1 BAD-TRANSACTION-1 EX-TRANSACTION-4)
                                        1000000))
              #false)
#|(check-expect
 (handle-block v2 REALER-BLOCK-2)
 (make-old-validator-state
  (make-hash
   (list (list (string-append
                "AAAAB3NzaC1yc2EAAAADAQABAAAAQQDKzzn3n3b6MDsAKZcWPzS5CRcpsnnYpL8L7MobUPfAmsDV3isn"
                "YPlmA/WM0FtbSGOU2tW6mYZOYaVhjOHrrCsZ")
               10000225)
         (list (string-append
                "AAAAB3NzaC1yc2EAAAADAQABAAAAQQCsuMztsNR31zDR0gmCroXg/uWZAJR7m3j2DTlb4t4APwENU"
                "tB3ERZk5Mxc/0gqftHGd8BK8x4Sl5r4CK4y23WD")
               999999999875)))
  (list REALER-BLOCK-2 REALER-BLOCK-1)
  '()))
|#


#|
;; show-state-handler : ValidatorState -> String
;; Converts a ValidatorState to a String for display and debugging.
(define (show-state-handler vs)
  (string-append "Ledger: " (map second validator-state-ledger vs)
                 "\nBlockchain: " (validator-state-blockchain vs)
                 "\nUnprocessed Transactions: " (validator-state-unproc-transactions vs)))
|#

(define (go init-state)
  (blockchain-big-bang init-state [on-transaction handle-transaction] [on-block handle-block]))

;;(go v1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MINING

(require racket/string)

;; block->string : Block -> String
;; Serializes a block into a string with the format.
(define (block->string blk)
  (local [(define transactions (block-transactions blk))
          (define transaction-strings
            (map (lambda (t) (string-replace (transaction->string t) ":" ";")) transactions))
          (define transaction-string (string-join transaction-strings ":"))]
         (format "block:~a:~a:~a" (block-nonce blk) (block-miner-key blk) transaction-string)))

;; mine+validate : ValidatorState PublicKey Number -> Boolean
;;
;; (mine+validate state miner-key retries)
;;
;; Uses mine-block (from Part 1) to mine the pending transactions in
;; the validator state.
;;
;; Produces #false if the retries are exhausted or if the number of pending
;; transactions is less than three.
;;
;; If mining succeeds, sends the serialized block using post-data and produces
;; #true.
(define (mine+validate state miner-key retries)
  (local [;; filter-transactions : ValidatorState -> Transaction
          ;; Creates a temporary ledger, and filters out all the transactions in the pending-transactions
          ;; that return false from update-ledger/transaction, while updating the temporary ledger.
          (define (filter-transactions st)
            (local
              [(define txs (validator-state-unproc-transactions st))
               (define txs-values (hash-values txs))]
              (first (foldl
                      (lambda (tx acc)
                        (local [(define lst (first acc))
                                (define ledger (second acc))
                                (define updated-ledger (update-ledger/transaction tx ledger))]
                          (if (false? updated-ledger)
                              acc
                              (list (cons tx lst) updated-ledger))))
                      (list '() (validator-state-ledger st))
                      txs-values))))
          ;; "block" is the mined block.
          (define block
            (mine-block (validator-state-last-digest state) miner-key (filter-transactions state) retries))]
    (if (and (not (boolean? block)) (>= (length (filter-transactions state)) 3))
        (post-data "accelchain.api.breq.dev" "/" (block->string block))
        #false)))

;(check-expect (mine+validate nv2 "exkey" 10) #false)

;; go-miner : ValidatorState PublicId Number -> ValidatorState
;;
;; (go-miner state miner-key retries) mines the pending transactions in state
;; uses `go` to validate the current blockchain, and then recurs indefinitely.
(define (go-miner state miner-key retries)
  (local [(define new-state (go state))
          (define new-block (mine+validate new-state miner-key retries))]
         (go-miner new-state miner-key retries)))

(go-miner start-state MY-PUBLIC-KEY 1000000)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
