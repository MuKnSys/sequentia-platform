;;; -*- Gerbil -*-
(import 
  :std/contract
  :std/error
  :std/misc/process
  :std/net/json-rpc
  :std/net/request
  :std/sugar
  :std/text/json
  ./bitcoin-client
  ./types)
(export #t)

(defclass (ElementsClient BitcoinClient) ())

(defmethod {daemon-executable-name ElementsClient}
  (lambda (self) "elementsd"))

(defmethod {cli-executable-name ElementsClient}
  (lambda (self) "elements-cli"))

(defmethod {create-raw-transaction ElementsClient}
  (lambda (self inputs outputs
    locktime: (locktime 0)
    replaceable: (replaceable #false))
    {run-json-rpc self "createrawtransaction" [inputs outputs locktime replaceable]}))

(defmethod {send-to-address ElementsClient} 
  (lambda (self address amount 
      comment: (comment #!void)
      comment-to: (comment-to #!void) 
      subtract-fee-from-amount: (subtract-fee-from-amount #false) 
      replaceable: (replaceable #!void)
      conf-target: (conf-target #!void)
      estimate-mode: (estimate-mode "unset")
      avoid-reuse: (avoid-reuse #!void)
      asset-label: (asset-label #!void)
      ignore-blind-fail: (ignore-blind-fail #!void)
      fee-rate: (fee-rate #!void)
      verbose: (verbose #!void))
    {run-json-rpc self "sendtoaddress" [address amount comment comment-to subtract-fee-from-amount replaceable conf-target estimate-mode avoid-reuse asset-label ignore-blind-fail fee-rate verbose]}))


(defmethod {dump-asset-labels ElementsClient}
  (lambda (self)
    {run-json-rpc self "dumpassetlabels" []}))

(defmethod {default-contract-hash ElementsClient}
  (lambda (self) (make-string 64 #\0)))

(defmethod {issue-asset ElementsClient}
  (lambda (self asset-amount token-amount 
      blind: (blind #false) 
      contract-hash: (contract-hash {default-contract-hash self}))
    {run-json-rpc self "issueasset" [asset-amount token-amount blind contract-hash]}))

(defmethod {raw-issue-asset ElementsClient}
  (lambda (self transaction issuances)
    {run-json-rpc self "rawissueasset" [transaction issuances]}))

(defmethod {list-issuances ElementsClient}
  (lambda (self)
    {run-json-rpc self "listissuances" []}))

(defmethod {reissue-asset ElementsClient}
  (lambda (self asset amount)
    {run-json-rpc self "reissueasset" [asset amount]}))
