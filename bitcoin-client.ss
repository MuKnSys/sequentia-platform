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

(set! std/net/request#request-response-bytes 
  (lambda (req)
    (try
      (if (eq? (request-status req) 200)
        (request-content req)
        (error "HTTP request failed"
          (request-status req) (request-status-text req) (ignore-errors (request-text req))))
      (finally
        (request-close req)))))

; Bitcoin
(defclass BitcoinClient 
  (data-directory options host port username password log-file))

(defmethod {daemon-executable-name BitcoinClient}
  (lambda (self) "bitcoind"))

(defmethod {cli-executable-name BitcoinClient}
  (lambda (self) "bitcoin-cli"))

(defmethod {rpc-server-url BitcoinClient}
  (lambda (self) (string-append (@ self host) ":" (number->string (@ self port)))))

; Daemon
(defmethod {is-daemon-running? BitcoinClient}
  (lambda (self) 
    (with-catch
      (lambda (exception) #false)
      (lambda () (http-get {rpc-server-url self}) #true))))

(defmethod {start-daemon BitcoinClient}
  (lambda (self)
    (run-process 
      (append 
        [{daemon-executable-name self}
         (string-append "-datadir=" (@ self data-directory))
         (string-append "-debuglogfile=" (@ self log-file))]
         (@ self options)))
      stdout-redirection: #false))

(defmethod {stop-daemon BitcoinClient}
  (lambda (self)
    (when {is-daemon-running? self}
      {run-json-rpc self "stop" []})))

(defmethod {restart-daemon BitcoinClient}
  (lambda (self)
    (when {is-daemon-running? self}
      {stop-daemon self})
    (thread-sleep! 5)
    {start-daemon self}
    (thread-sleep! 5)))

(defmethod {get-rpc-info BitcoinClient}
  (lambda (self)
    {run-json-rpc self "getrpcinfo" []}))

; JSON-RPC
(defmethod {run-json-rpc BitcoinClient}
  (lambda (self method-name params)
    (json-rpc {rpc-server-url self} method-name params
      auth: [basic: (@ self username) (@ self password)]
      result-decoder: identity
      log: (lambda (log) (displayln log)))))

; CLI
(defmethod {run-cli-command BitcoinClient}
  (lambda (self arguments)
    (run-process [{cli-executable-name self} 
      (string-append "-datadir=" (@ self data-directory))
      "-daemon"
      arguments ...]
      stdout-redirection: #f)))

; Blockchain state
(defmethod {get-block-count BitcoinClient}
  (lambda (self)
    {run-json-rpc self "getblockcount" []}))

(defmethod {get-block BitcoinClient}
  (lambda (self block-hash
      verbosity: (verbosity 1))
    {run-json-rpc self "getblock" [block-hash verbosity]}))

; Transactions
(defmethod {create-raw-transaction BitcoinClient}
  (lambda (self inputs outputs 
    locktime: (locktime #!void) 
    replaceable: (replaceable #!void))
  {run-json-rpc self "createrawtransaction" [inputs outputs locktime replaceable]}))

(defmethod {sign-raw-transaction-with-key BitcoinClient}
  (lambda (self) #f))

(defmethod {send-raw-transaction BitcoinClient} 
  (lambda (self hex)
    {run-json-rpc self "sendrawtransaction" [hex]}))

(defmethod {sign-raw-transaction-with-wallet BitcoinClient}
  (lambda (self hex-string
      prev-txs: (prev-txs #!void))
    {run-json-rpc self "signrawtransactionwithwallet" [hex-string prev-txs]}))

(defmethod {decode-raw-transaction BitcoinClient}
  (lambda (self hex-string
      is-witness: (is-witness #!void))
    ; TODO: Correctly handle presence and absence of is-witness argument
    {run-json-rpc self "decoderawtransaction" [hex-string]}))

(defmethod {fund-raw-transaction BitcoinClient}
  (lambda (self hex-string
      options: (options #!void)
      is-witness: (is-witness #!void))
    ; TODO: Correctly handle presence and absence of is-witness argument
    {run-json-rpc self "fundrawtransaction" [hex-string]}))

(defmethod {get-transaction BitcoinClient}
  (lambda (self tx-id
      include-watch-only: (include-watch-only #!void)
      verbose: (verbose #!void))
    {run-json-rpc self "gettransaction" [tx-id include-watch-only verbose]}))

; Wallet
(defmethod {initialize-wallet BitcoinClient}
  (lambda (self
      wallet-name: (wallet-name ""))
    (unless {has-wallet? self wallet-name}
      {create-wallet self wallet-name})
    (unless {is-wallet-loaded? self}
      {load-wallet self wallet-name})))

(defmethod {has-wallet? BitcoinClient}
  (lambda (self name) 
    (def existing-wallet-names {list-wallet-dir self})
    (find (lambda (existing-wallet-name) (equal? name existing-wallet-name)) existing-wallet-names)))

(defmethod {is-wallet-loaded? BitcoinClient}
  (lambda (self) 
    (not (null? {list-wallets self}))))

(defmethod {list-wallets BitcoinClient}
  (lambda (self)
    {run-json-rpc self "listwallets" []}))

(defmethod {list-wallet-dir BitcoinClient}
  (lambda (self)
    (def result {run-json-rpc self "listwalletdir" []})
    (def wallets (hash-get result "wallets"))
    (map (lambda (wallet) (hash-get wallet "name")) wallets)))

(defmethod {get-wallet-info BitcoinClient}
  (lambda (self)
    {run-json-rpc self "getwalletinfo" []}))

(defmethod {create-wallet BitcoinClient}
  (lambda (self name)
    ; See https://github.com/ElementsProject/elements/issues/1106 for why extra parameters are necessary
    (def result {run-json-rpc self "createwallet" [name #false #false "" #false #false]})
    (def warning (hash-get result "warning"))
    (when warning (displayln (string-append "WARNING: " warning)))))

(defmethod {load-wallet BitcoinClient}
  (lambda (self filename
      load-on-startup: (load-on-startup #!void))
    {run-json-rpc self "loadwallet" [filename]}))

(defmethod {unload-wallet BitcoinClient}
  (lambda (self filename
      load-on-startup: (load-on-startup #!void))
    {run-json-rpc self "unloadwallet" [filename]}))

(defmethod {rescan-blockchain BitcoinClient}
  (lambda (self
      start-height: (start-height #!void)
      stop-height: (stop-height #!void))
    {run-json-rpc self "rescanblockchain" [start-height]}))

(defmethod {get-new-address BitcoinClient} 
  (lambda (self 
      label: (label #!void) 
      address-type: (address-type #!void))
    {run-json-rpc self "getnewaddress" [label address-type]}))

(defmethod {get-raw-change-address BitcoinClient} 
  (lambda (self
      address-type: (address-type #!void))
    {run-json-rpc self "getrawchangeaddress" [address-type]}))

(defmethod {send-to-address BitcoinClient} 
  (lambda (self address amount 
      comment: (comment #!void)
      comment-to: (comment-to #!void) 
      subtract-fee-from-amount: (subtract-fee-from-amount #!void) 
      replaceable: (replaceable #!void)
      conf-target: (conf-target #!void)
      estimate-mode: (estimate-mode #!void)
      avoid-reuse: (avoid-reuse #!void))
    {run-json-rpc self "sendtoaddress" [address amount comment comment-to subtract-fee-from-amount replaceable conf-target estimate-mode avoid-reuse]}))

(defmethod {generate-to-address BitcoinClient}
  (lambda (self n-blocks address
      max-tries: (max-tries #!void))
    {run-json-rpc self "generatetoaddress" [n-blocks address max-tries]}))

(defmethod {list-unspent BitcoinClient}
  (lambda (self 
      min-conf: (min-conf #!void)
      max-conf: (max-conf #!void)
      addresses: (addresses #!void)
      include-unsafe: (include-unsafe #!void)
      query-options: (query-options #!void))
    (def utxo-json-objects {run-json-rpc self "listunspent" [min-conf max-conf addresses include-unsafe query-options]})
    (map (cut trivial-json-object->class Utxo::t <>) utxo-json-objects)))

(defmethod {get-balance BitcoinClient}
  (lambda (self
      min-conf: (min-conf #!void)
      include-watch-only: (include-watch-only #!void)
      avoid-reuse: (avoid-reuse #!void))
    {run-json-rpc self "getbalance" ["*" min-conf include-watch-only avoid-reuse]}))

(defmethod {get-balances BitcoinClient}
  (lambda (self)
    {run-json-rpc self "getbalances" []}))
