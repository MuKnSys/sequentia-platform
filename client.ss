;;; -*- Gerbil -*-
(import
  :std/contract
  :std/error
  :std/event
  :std/misc/process
  :std/net/json-rpc
  :std/net/request
  :std/sugar
  :std/text/json
  ./types)
(export #t)

; Sequentia
(defclass SequentiaClient
  (data-directory host port username password log-file log-rpc? daemon-options))

(defmethod {daemon-executable-name SequentiaClient}
  (lambda (self) "elementsd"))

(defmethod {cli-executable-name SequentiaClient}
  (lambda (self) "elements-cli"))

(defmethod {rpc-server-url SequentiaClient}
  (lambda (self) (string-append (@ self host) ":" (number->string (@ self port)))))

; Daemon
(defmethod {is-daemon-running? SequentiaClient}
  (lambda (self)
    (with-catch
      (lambda (exception) #false)
      (lambda () (http-get {rpc-server-url self}) #true))))

(defmethod {start-daemon SequentiaClient}
  (lambda (self)
    (run-process
      (append
        [{daemon-executable-name self}
         (string-append "-datadir=" (@ self data-directory))
         (string-append "-debuglogfile=" (@ self log-file))]
         (@ self daemon-options)))
      stdout-redirection: #false))

(defmethod {stop-daemon SequentiaClient}
  (lambda (self)
    (when {is-daemon-running? self}
      {run-json-rpc self "stop" []})))

(defmethod {restart-daemon SequentiaClient}
  (lambda (self)
    (when {is-daemon-running? self}
      {stop-daemon self})
    (thread-sleep! 5)
    {start-daemon self}
    (thread-sleep! 5)))

(defmethod {get-rpc-info SequentiaClient}
  (lambda (self)
    {run-json-rpc self "getrpcinfo" []}))

; JSON-RPC
(defmethod {run-json-rpc SequentiaClient}
  (lambda (self method-name params)
    (json-rpc {rpc-server-url self} method-name params
      auth: [basic: (@ self username) (@ self password)]
      result-decoder: identity
      log: (lambda (log) (when (@ self log-rpc?) (displayln log))))))

; CLI
(defmethod {run-cli-command SequentiaClient}
  (lambda (self arguments)
    (run-process [{cli-executable-name self}
      (string-append "-datadir=" (@ self data-directory))
      "-daemon"
      arguments ...]
      stdout-redirection: #f)))

; Blockchain
(defmethod {get-block-count SequentiaClient}
  (lambda (self)
    {run-json-rpc self "getblockcount" []}))

(defmethod {get-block SequentiaClient}
  (lambda (self block-hash
      verbosity: (verbosity 1))
    {run-json-rpc self "getblock" [block-hash verbosity]}))

(defmethod {get-mempool-ancestors SequentiaClient}
  (lambda (self tx-id
      verbosity: (verbosity #false))
    {run-json-rpc self "getmempoolancestors" [tx-id verbosity]}))

(defmethod {get-mempool-descendants SequentiaClient}
  (lambda (self tx-id
      verbosity: (verbosity #false))
    {run-json-rpc self "getmempooldescendants" [tx-id verbosity]}))

(defmethod {get-mempool-entry SequentiaClient}
  (lambda (self tx-id)
    {run-json-rpc self "getmempoolentry" [tx-id]}))

(defmethod {get-mempool-info SequentiaClient}
  (lambda (self)
    {run-json-rpc self "getmempoolinfo" []}))

(defmethod {get-raw-mempool SequentiaClient}
  (lambda (self)
    {run-json-rpc self "getrawmempool" []}))

(defmethod {save-mempool SequentiaClient}
  (lambda (self)
    {run-json-rpc self "savemempool" []}))

; Transactions
(defmethod {create-raw-transaction SequentiaClient}
  (lambda (self inputs outputs
    locktime: (locktime 0)
    replaceable: (replaceable #false))
    {run-json-rpc self "createrawtransaction" [inputs outputs locktime replaceable]}))

(defmethod {sign-raw-transaction-with-key SequentiaClient}
  (lambda (self) #f))

(defmethod {send-raw-transaction SequentiaClient}
  (lambda (self hex)
    {run-json-rpc self "sendrawtransaction" [hex]}))

(defmethod {sign-raw-transaction-with-wallet SequentiaClient}
  (lambda (self hex-string
      prev-txs: (prev-txs #!void))
    {run-json-rpc self "signrawtransactionwithwallet" [hex-string prev-txs]}))

(defmethod {decode-raw-transaction SequentiaClient}
  (lambda (self hex-string
      is-witness: (is-witness #!void))
    ; TODO: Correctly handle presence and absence of is-witness argument
    {run-json-rpc self "decoderawtransaction" [hex-string]}))

(defmethod {fund-raw-transaction SequentiaClient}
  (lambda (self hex-string
      options: (options #!void)
      is-witness: (is-witness #!void))
    ; TODO: Correctly handle presence and absence of is-witness argument
    {run-json-rpc self "fundrawtransaction" [hex-string]}))

(defmethod {get-transaction SequentiaClient}
  (lambda (self tx-id
      include-watch-only: (include-watch-only #!void)
      verbose: (verbose #!void))
    {run-json-rpc self "gettransaction" [tx-id include-watch-only verbose]}))

(defmethod {get-raw-transaction SequentiaClient}
  (lambda (self tx-id
      verbose: (verbose #!void))
    {run-json-rpc self "getrawtransaction" [tx-id verbose]}))

(defmethod {list-transactions SequentiaClient}
  (lambda (self
      label: (label "*")
      count: (count 10)
      skip: (skip 0)
      include-watch-only: (include-watch-only #false))
    {run-json-rpc self "listtransactions" [label count skip include-watch-only]}))

(defmethod {blind-raw-transaction SequentiaClient}
  (lambda (self hex
    ignore-blind-fail: (ignore-blind-fail #!void)
    asset-commitments: (asset-commitments #!void)
    blind-issuances: (blind-issuances #!void)
    total-blinder: (total-blinder #!void))
    {run-json-rpc self "blindrawtransaction" [hex ignore-blind-fail asset-commitments blind-issuances total-blinder]}))

; Wallet
(defmethod {initialize-wallet SequentiaClient}
  (lambda (self
      wallet-name: (wallet-name ""))
    (unless {has-wallet? self wallet-name}
      {create-wallet self wallet-name})
    (unless {is-wallet-loaded? self}
      {load-wallet self wallet-name})))

(defmethod {has-wallet? SequentiaClient}
  (lambda (self name)
    (def existing-wallet-names {list-wallet-dir self})
    (find (lambda (existing-wallet-name) (equal? name existing-wallet-name)) existing-wallet-names)))

(defmethod {is-wallet-loaded? SequentiaClient}
  (lambda (self)
    (not (null? {list-wallets self}))))

(defmethod {list-wallets SequentiaClient}
  (lambda (self)
    {run-json-rpc self "listwallets" []}))

(defmethod {list-wallet-dir SequentiaClient}
  (lambda (self)
    (def result {run-json-rpc self "listwalletdir" []})
    (def wallets (hash-get result "wallets"))
    (map (lambda (wallet) (hash-get wallet "name")) wallets)))

(defmethod {get-wallet-info SequentiaClient}
  (lambda (self)
    {run-json-rpc self "getwalletinfo" []}))

(defmethod {create-wallet SequentiaClient}
  (lambda (self name)
    {run-json-rpc self "createwallet" [name #false #false "" #false #false]}))

(defmethod {load-wallet SequentiaClient}
  (lambda (self filename
      load-on-startup: (load-on-startup #!void))
    {run-json-rpc self "loadwallet" [filename]}))

(defmethod {unload-wallet SequentiaClient}
  (lambda (self filename
      load-on-startup: (load-on-startup #!void))
    {run-json-rpc self "unloadwallet" [filename]}))

(defmethod {rescan-blockchain SequentiaClient}
  (lambda (self
      start-height: (start-height #!void)
      stop-height: (stop-height #!void))
    {run-json-rpc self "rescanblockchain" [start-height]}))

(defmethod {get-new-address SequentiaClient}
  (lambda (self
      label: (label #!void)
      address-type: (address-type #!void))
    {run-json-rpc self "getnewaddress" [label address-type]}))

(defmethod {get-raw-change-address SequentiaClient}
  (lambda (self
      address-type: (address-type #!void))
    {run-json-rpc self "getrawchangeaddress" [address-type]}))

(defmethod {send-to-address SequentiaClient}
  (lambda (self address amount
      comment: (comment #!void)
      comment-to: (comment-to #!void)
      subtract-fee-from-amount: (subtract-fee-from-amount #!void)
      replaceable: (replaceable #!void)
      conf-target: (conf-target #!void)
      estimate-mode: (estimate-mode #!void)
      avoid-reuse: (avoid-reuse #!void)
      asset-label: (asset-label #!void)
      ignore-blind-fail: (ignore-blind-fail #!void)
      fee-rate: (fee-rate #!void)
      fee-asset-label: (fee-asset-label #!void)
      verbose: (verbose #!void))
    {run-json-rpc self "sendtoaddress"
      [address amount comment comment-to subtract-fee-from-amount replaceable conf-target estimate-mode avoid-reuse asset-label ignore-blind-fail fee-rate fee-asset-label verbose]}))

(defmethod {generate-to-address SequentiaClient}
  (lambda (self n-blocks address
      max-tries: (max-tries #!void))
    {run-json-rpc self "generatetoaddress" [n-blocks address max-tries]}))

(defmethod {list-unspent SequentiaClient}
  (lambda (self
      min-conf: (min-conf #!void)
      max-conf: (max-conf #!void)
      addresses: (addresses #!void)
      include-unsafe: (include-unsafe #!void)
      query-options: (query-options #!void))
    (def utxo-json-objects {run-json-rpc self "listunspent" [min-conf max-conf addresses include-unsafe query-options]})
    (map (cut trivial-json-object->class Utxo::t <>) utxo-json-objects)))

(defmethod {get-balance SequentiaClient}
  (lambda (self
      min-conf: (min-conf #!void)
      include-watch-only: (include-watch-only #!void)
      avoid-reuse: (avoid-reuse #!void))
    {run-json-rpc self "getbalance" ["*" min-conf include-watch-only avoid-reuse]}))

(defmethod {get-balances SequentiaClient}
  (lambda (self)
    {run-json-rpc self "getbalances" []}))

;; Assets
(defmethod {dump-asset-labels SequentiaClient}
  (lambda (self)
    {run-json-rpc self "dumpassetlabels" []}))

(defmethod {default-contract-hash SequentiaClient}
  (lambda (self) (make-string 64 #\0)))

(defmethod {issue-asset SequentiaClient}
  (lambda (self asset-amount token-amount
      blind: (blind #false)
      contract-hash: (contract-hash (make-string 64 #\0)))
    {run-json-rpc self "issueasset" [asset-amount token-amount blind contract-hash]}))

(defmethod {raw-issue-asset SequentiaClient}
  (lambda (self transaction issuances)
    {run-json-rpc self "rawissueasset" [transaction issuances]}))

(defmethod {list-issuances SequentiaClient}
  (lambda (self)
    {run-json-rpc self "listissuances" []}))

(defmethod {reissue-asset SequentiaClient}
  (lambda (self asset amount)
    {run-json-rpc self "reissueasset" [asset amount]}))

;; Exchange rates
(defmethod {get-fee-exchange-rates SequentiaClient}
  (lambda (self)
    {run-json-rpc self "getfeeexchangerates" []}))

(defmethod {set-fee-exchange-rates SequentiaClient}
  (lambda (self rates)
    {run-json-rpc self "setfeeexchangerates" [rates]}))
