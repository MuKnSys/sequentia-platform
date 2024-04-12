#!/usr/bin/env gxi

(import
  :std/assert
  :std/cli/getopt
  :std/cli/multicall
  :std/misc/process
  :std/sugar
  :std/text/json
  :mukn/sequentia/sequentia-client
  :mukn/sequentia/test-client
  :mukn/sequentia/types)
 
(def (setup)
  (set! (@ client log-rpc?) #true)
  {restart-daemon client}
  {initialize-wallet client}
  {rescan-blockchain client})

(def (teardown)
  {stop-daemon client}
  (def database-path (string-append (@ client data-directory) "/elementsregtest"))
  (when (file-exists? database-path)
    (run-process ["rm" "-rf" database-path])))

(def (run-test block) 
  (setup)
  (block)
  (teardown))

; Test scenarios
(define-entry-point (normal-transaction)
  (help: "Run test scenario for normal transaction" getopt: [])
  (run-test (lambda ()
    (def send-address {get-new-address client address-type: "bech32"})
    (def receive-address {get-new-address client address-type: "bech32"})
    (def change-address {get-new-address client address-type: "bech32"})
    (def bitcoin (hash-get {dump-asset-labels client} "bitcoin"))
    (def utxo (last {list-unspent client}))
    (def inputs
      [(make-TxInput txid: (@ utxo txid) vout: (@ utxo vout) sequence: #!void)])
    (def outputs
      [(make-TxAddressOutput address: receive-address amount: 10 asset: bitcoin)
      (make-TxAddressOutput address: change-address amount: (- (@ utxo amount) 10 0.01) asset: bitcoin)
      (make-TxFeeOutput amount: 0.01)])
    (def raw-tx {create-raw-transaction client inputs outputs})
    (def tx {decode-raw-transaction client raw-tx})
    (def signed-raw-tx (hash-get {sign-raw-transaction-with-wallet client raw-tx} "hex"))
    {send-raw-transaction client signed-raw-tx})))

(define-entry-point (fund-transaction)
  (help: "Run test scenario for fund transaction" getopt: [])
  (run-test (lambda ()
    (def send-address {get-new-address client address-type: "bech32"})
    (def receive-address {get-new-address client address-type: "bech32"})
    (def change-address {get-new-address client address-type: "bech32"})
    (def bitcoin (hash-get {dump-asset-labels client} "bitcoin"))
    (def utxo (last {list-unspent client}))
    (def outputs
      [(make-TxAddressOutput address: receive-address amount: 10 asset: bitcoin)
       (make-TxAddressOutput address: change-address amount: (- (@ utxo amount) 10 0.01) asset: bitcoin)
       (make-TxFeeOutput amount: 0.01)])
    (def raw-tx {create-raw-transaction client [] outputs})
    (def funded-raw-tx (hash-get {fund-raw-transaction client raw-tx} "hex"))
    (def signed-funded-raw-tx (hash-get {sign-raw-transaction-with-wallet client funded-raw-tx} "hex"))
    {send-raw-transaction client signed-funded-raw-tx})))

(define-entry-point (zero-fee-transaction)
  (help: "Run test scenario for zero fee transaction" getopt: [])
  (run-test (lambda ()
    {rescan-blockchain client}
    {get-balances client}
    (def send-address {get-new-address client address-type: "bech32"})
    (def receive-address {get-new-address client address-type: "bech32"})
    (def block-hashes {generate-to-address client 100 send-address})
    {rescan-blockchain client}
    (def utxos {list-unspent client})
    (def utxo (last utxos))
    (def bitcoin (hash-get {dump-asset-labels client} "bitcoin"))
    (def inputs
      [(make-TxInput txid: (@ utxo txid) vout: (@ utxo vout) sequence: #!void)])
    (def outputs
      [(make-TxAddressOutput address: receive-address amount: (@ utxo amount) asset: bitcoin)
      (make-TxFeeOutput amount: 0)])
    (def raw-tx {create-raw-transaction client inputs outputs})
    (def tx {decode-raw-transaction client raw-tx})
    (def signed-raw-tx (hash-get {sign-raw-transaction-with-wallet client raw-tx} "hex"))
    {send-raw-transaction client signed-raw-tx})))


(define-entry-point (zero-fee-issuance)
  (help: "Run test scenario for zero fee issuance" getopt: [])
  (run-test (lambda ()
    (def send-address {get-new-address client address-type: "bech32"})
    (def receive-address {get-new-address client address-type: "bech32"})
    (def asset-address {get-new-address client address-type: "bech32"})
    (def block-hashes {generate-to-address client 100 send-address})
    (def utxos {list-unspent client})
    (def utxo (last utxos))
    (def bitcoin (hash-get {dump-asset-labels client} "bitcoin"))
    (def inputs
      [(make-TxInput txid: (@ utxo txid) vout: (@ utxo vout) sequence: #!void)])
    (def outputs
      [(make-TxAddressOutput address: receive-address amount: (@ utxo amount) asset: bitcoin)
      (make-TxFeeOutput amount: 0)])
    (def hex {create-raw-transaction client inputs outputs})
    (def tx {decode-raw-transaction client hex})
    (def issuance (make-Issuance
      asset_amount: 1000
      asset_address: asset-address
      token_amount: #!void
      token_address: #!void
      blind: #false
      contract_hash: {default-contract-hash client}))
    {raw-issue-asset client hex [issuance]})))

(define-entry-point (zero-input-issuance)
  (help: "Run test scenario for zero input issuance" getopt: [])
  (run-test (lambda ()
    (def asset-address {get-new-address client address-type: "bech32"})
    (def raw-tx {create-raw-transaction client [] []})
    {decode-raw-transaction client raw-tx}
    (def issuance (make-Issuance
      asset_amount: 1000
      asset_address: asset-address
      token_amount: #!void
      token_address: #!void
      blind: #false
      contract_hash: {default-contract-hash client}))
    {raw-issue-asset client raw-tx [issuance]})))

(define-entry-point (custom-asset-transaction)
  (help: "Run test scenario for custom asset transaction" getopt: [])
  (run-test (lambda ()

    (displayln "Create asset")
    (def asset {issue-asset client 10 0})
    (def asset-hex (hash-get asset "asset"))

    (displayln "Generate block")
    (def funding-address {get-new-address client address-type: "bech32"})
    {generate-to-address client 1 funding-address}
    {rescan-blockchain client}

    (displayln "Pay fee with bitcoin")
    (def utxos {list-unspent client})
    (def bitcoin-hex (hash-get {dump-asset-labels client} "bitcoin"))
    (def bitcoin-utxo (find (lambda (utxo) (equal? (@ utxo asset) bitcoin-hex)) utxos))
    (def asset-utxo (find (lambda (utxo) (equal? (@ utxo asset) asset-hex)) utxos))
    (def destination-address {get-new-address client address-type: "bech32"})
    (def change-address {get-raw-change-address client address-type: "bech32"})
    (def inputs
      [(make-TxInput txid: (@ asset-utxo txid) vout: (@ asset-utxo vout) sequence: #!void)
      (make-TxInput txid: (@ bitcoin-utxo txid) vout: (@ bitcoin-utxo vout) sequence: #!void)])
    (def outputs
      [(make-TxAddressOutput address: destination-address amount: (@ asset-utxo amount) asset: asset-hex)
      (make-TxAddressOutput address: change-address amount: (- (@ bitcoin-utxo amount) 0.01) asset: bitcoin-hex)
      (make-TxAnyFeeOutput amount: 0.01 asset: bitcoin-hex)])
    (def raw-tx {create-raw-transaction client inputs outputs})
    (def signed-raw-tx (hash-get {sign-raw-transaction-with-wallet client raw-tx} "hex"))
    {send-raw-transaction client signed-raw-tx})))

(define-entry-point (raw-no-coin-transaction)
  (help: "Run test scenario for raw no coin transaction" getopt: [])
  (run-test (lambda ()

    (displayln "Create asset")
    (def asset {issue-asset client 100 0})
    (def asset-hex (hash-get asset "asset"))

    (displayln "Generate block")
    (def funding-address {get-new-address client address-type: "blech32"})
    {generate-to-address client 1 funding-address}
    {rescan-blockchain client}

    (displayln "Pay fee with new asset")
    (def utxos {list-unspent client})
    (def utxo (find (lambda (utxo) (equal? (@ utxo asset) asset-hex)) utxos))
    (def destination-address {get-new-address client address-type: "blech32"})
    (def inputs
      [(make-TxInput txid: (@ utxo txid) vout: (@ utxo vout) sequence: #!void)])
    (def outputs
      [(make-TxAddressOutput address: destination-address amount: (- (@ utxo amount) 0.01) asset: asset-hex)
      (make-TxAnyFeeOutput amount: 0.01 asset: asset-hex)])
    (def raw-tx {create-raw-transaction client inputs outputs})
    (def blinded-raw-tx {blind-raw-transaction client raw-tx})
    (def signed-raw-tx (hash-get {sign-raw-transaction-with-wallet client blinded-raw-tx} "hex"))
    {send-raw-transaction client signed-raw-tx}
      
    (displayln "Pay out rewards")
    (def rewards-address {get-new-address client address-type: "blech32"})
    {generate-to-address client 101 rewards-address}
    {rescan-blockchain client}
    (def rewards {list-unspent client addresses: [rewards-address]})
    (assert! (= (length rewards) 1))
    (def reward-utxo (car rewards))
    (assert! (equal? (@ reward-utxo asset) asset-hex)))))

(define-entry-point (no-coin-transaction)
  (help: "Run test scenario for no coin transaction" getopt: [])
  (run-test (lambda ()

    (displayln "Generate asset")
    (def asset {issue-asset client 100 0})
    (def asset-hex (hash-get asset "asset"))
    {rescan-blockchain client}
    {get-balances client}

    (displayln "Generate block")
    (def funding-address {get-new-address client address-type: "bech32"})
    {generate-to-address client 1 funding-address}
    {rescan-blockchain client}
    {get-balances client}

    (displayln "Pay fee with new asset")
    (def destination-address {get-new-address client address-type: "bech32"})
    {send-to-address client destination-address 1 asset-label: asset-hex}
    {send-to-address client destination-address 1 asset-label: "genesis"}

    (displayln "Pay out rewards")
    (def rewards-address {get-new-address client address-type: "bech32"})
    {generate-to-address client 101 rewards-address}
    {rescan-blockchain client}
    (def rewards {list-unspent client addresses: [rewards-address]})
    (assert! (= (length rewards) 2))
    (def reward-utxo (car rewards))
    (assert! (equal? (@ reward-utxo asset) asset-hex)))))

(define-entry-point (set-fee-exchange-rates)
  (help: "Run test scenario for setting fee exchange rates" getopt: [])
  (run-test (lambda ()
    (def asset "0000000000000000000000000000000000000000000000000000000000000001")
    (def rate 2000000000)
    (def new-rates (list->hash-table [(cons asset rate)]))
    {set-fee-exchange-rates client new-rates}
    (def rates {get-fee-exchange-rates client})
    (assert! (equal? (hash-get rates asset) rate)))))

(define-entry-point (raw-issue-asset)
  (help: "Run test scenario for raw asset issuance" getopt: [])
  (run-test (lambda ()
    (displayln "Fund wallet")
    (def funding-address {get-new-address client address-type: "bech32"})
    {generate-to-address client 101 funding-address}
    
    (displayln "Create asset")
    (def asset-address {get-new-address client address-type: "bech32"})
    (def token-address {get-new-address client address-type: "bech32"})
    (def raw-issuance-tx {create-raw-transaction client [] [(make-TxDataOutput data: "00")]})
    (def funded-raw-issuance-tx (hash-get {fund-raw-transaction client raw-issuance-tx} "hex"))
    (def issuance (make-Issuance
      asset_amount: 33
      asset_address: asset-address
      token_amount: 7
      token_address: token-address
      blind: #false))
    (def asset (car {raw-issue-asset client funded-raw-issuance-tx [issuance]}))
    (def asset-tx (hash-get asset "hex"))
    (def signed-asset-tx (hash-get {sign-raw-transaction-with-wallet client asset-tx} "hex"))
    {send-raw-transaction client signed-asset-tx}
    (def asset-hex (hash-get asset "asset"))

    (displayln "Generate block")
    {generate-to-address client 1 funding-address}
    {rescan-blockchain client}

    (displayln "Pay fee with new asset")
    (def asset-utxo (find (lambda (utxo) (equal? (@ utxo asset) asset-hex)) {list-unspent client}))
    (def destination-address {get-new-address client address-type: "bech32"})
    (def inputs
      [(make-TxInput txid: (@ asset-utxo txid) vout: (@ asset-utxo vout) sequence: #!void)])
    (def outputs
      [(make-TxAddressOutput address: destination-address amount: (- (@ asset-utxo amount) 0.01) asset: asset-hex)
       (make-TxAnyFeeOutput amount: 0.01 asset: asset-hex)])
    (def raw-tx {create-raw-transaction client inputs outputs})
    (def signed-raw-tx (hash-get {sign-raw-transaction-with-wallet client raw-tx} "hex"))
    {send-raw-transaction client signed-raw-tx}
      
    (displayln "Pay out rewards")
    (def rewards-address {get-new-address client address-type: "bech32"})
    {generate-to-address client 1 rewards-address}
    {rescan-blockchain client}
    (def rewards {list-unspent client addresses: [rewards-address]})
    (assert! (= (length rewards) 1)))))

(current-program "test")
(set-default-entry-point! 'no-coin-transaction)
(define-multicall-main)
