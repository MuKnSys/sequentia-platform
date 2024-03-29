;;; -*- Gerbil -*-
(import 
  ./elements-client)
(export #t)

(defclass (SequentiaClient ElementsClient) (exchange-rates-json-file))

(defmethod {start-daemon ElementsClient}
  (lambda (self)
    (run-process 
      (append 
        [{daemon-executable-name self}
         (string-append "-datadir=" (@ self data-directory))
         (string-append "-debuglogfile=" (@ self log-file))
         (string-append "-exchangeratesjsonfile=" (@ self exchange-rates-json-file))]
         (@ self options)))
      stdout-redirection: #false))

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

(defmethod {get-fee-exchange-rates SequentiaClient} 
  (lambda (self)
    {run-json-rpc self "getfeeexchangerates" []}))

(defmethod {set-fee-exchange-rates SequentiaClient} 
  (lambda (self rates)
    {run-json-rpc self "setfeeexchangerates" [rates]}))
