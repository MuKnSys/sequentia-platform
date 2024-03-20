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
 
(def (initialize-test)
  {restart-daemon client}
  {initialize-wallet client}
  {rescan-blockchain client})

(def (deinitialize-test)
  {stop-daemon client}
  (def database-path (string-append (@ client data-directory) "/elementsregtest"))
  (when (file-exists? database-path)
    (run-process ["rm" "-rf" database-path])))

; Test scenarios
(define-entry-point (normal-transaction)
  (help: "Run test scenario for normal transaction" getopt: [])
  (initialize-test)
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
  {send-raw-transaction client signed-raw-tx}
  (deinitialize-test))

(define-entry-point (zero-fee-transaction)
  (help: "Run test scenario for zero fee transaction" getopt: [])
  (initialize-test)
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
  {send-raw-transaction client signed-raw-tx}
  (deinitialize-test))


(define-entry-point (zero-fee-issuance)
  (help: "Run test scenario for zero fee issuance" getopt: [])
  (initialize-test)
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
  {raw-issue-asset client hex [issuance]}
  (deinitialize-test))

(define-entry-point (zero-input-issuance)
  (help: "Run test scenario for zero input issuance" getopt: [])
  (initialize-test)
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
  {raw-issue-asset client raw-tx [issuance]}
  (deinitialize-test))

(define-entry-point (custom-asset-transaction)
  (help: "Run test scenario for custom asset transaction" getopt: [])
  (initialize-test)

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
  {send-raw-transaction client signed-raw-tx}
  (deinitialize-test))

(define-entry-point (raw-no-coin-transaction)
  (help: "Run test scenario for raw no coin transaction" getopt: [])
  (initialize-test)

  (displayln "Create asset")
  (def asset {issue-asset client 100 0})
  (def asset-hex (hash-get asset "asset"))

  (displayln "Generate block")
  (def funding-address {get-new-address client address-type: "bech32"})
  {generate-to-address client 1 funding-address}
  {send-to-address client funding-address 10 asset-label: asset-hex}
  {generate-to-address client 1 funding-address}
  {rescan-blockchain client}

  (displayln "Pay fee with new asset")
  (def utxos {list-unspent client addresses: [funding-address]})
  (def utxo (find (lambda (utxo) (equal? (@ utxo asset) asset-hex)) utxos))
  (def destination-address {get-new-address client address-type: "bech32"})
  (def inputs
    [(make-TxInput txid: (@ utxo txid) vout: (@ utxo vout) sequence: #!void)])
  (def outputs
    [(make-TxAddressOutput address: destination-address amount: (- (@ utxo amount) 0.01) asset: asset-hex)
     (make-TxAnyFeeOutput amount: 0.01 asset: asset-hex)])
  (def raw-tx {create-raw-transaction client inputs outputs})
  (def signed-raw-tx (hash-get {sign-raw-transaction-with-wallet client raw-tx} "hex"))
  {send-raw-transaction client signed-raw-tx}
    
  (displayln "Pay out rewards")
  (def rewards-address {get-new-address client address-type: "bech32"})
  {generate-to-address client 1 rewards-address}
  {rescan-blockchain client}
  (def rewards {list-unspent client addresses: [rewards-address]})
  (assert! (= (length rewards) 1))
  (deinitialize-test))

(define-entry-point (no-coin-transaction)
  (help: "Run test scenario for no coin transaction" getopt: [])
  (initialize-test)

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

  (displayln "Send asset to new address")
  (def bitcoin-hex (hash-get {dump-asset-labels client} "bitcoin"))
  {send-to-address client funding-address 1 asset-label: asset-hex fee-asset-label: bitcoin-hex}
  {generate-to-address client 1 funding-address}
  {rescan-blockchain client}
  {get-balances client}

  (displayln "Pay fee with new asset")
  (def destination-address {get-new-address client address-type: "bech32"})
  {send-to-address client destination-address 1 asset-label: asset-hex}

  (displayln "Pay out rewards")
  (def rewards-address {get-new-address client address-type: "bech32"})
  {generate-to-address client 101 rewards-address}
  {rescan-blockchain client}
  (def rewards {list-unspent client addresses: [rewards-address]})
  (assert! (= (length rewards) 1))
  (def reward-utxo (car rewards))
  (assert! (equal? (@ reward-utxo asset) asset-hex))
  (deinitialize-test))

(current-program "test-scenarios")
(set-default-entry-point! 'no-coin-transaction)
(define-multicall-main)
