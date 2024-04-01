#!/usr/bin/env gxi

(import
  :std/assert
  :std/cli/getopt
  :std/cli/multicall
  :std/misc/process
  :std/sugar
  :std/text/json
  :clan/string
  :mukn/sequentia/sequentia-client
  :mukn/sequentia/test-client
  :mukn/sequentia/types)
 
(def (setup)
  (set! (@ client log-rpc?) #false)
  {initialize-wallet client}
  {rescan-blockchain client})

(def (teardown)
  (def database-path (string-append (@ client data-directory) "/elementsregtest"))
  (when (file-exists? database-path)
    (run-process ["rm" "-rf" database-path])))

(def (run-demo block) 
  (setup)
  (block)
  (teardown))

(def HEADER "\033[95m")
(def OKBLUE "\033[94m")
(def OKCYAN "\033[96m")
(def OKGREEN "\033[92m")
(def WARNING "\033[93m")
(def FAIL "\033[91m")
(def ENDC "\033[0m")
(def BOLD "\033[1m")
(def UNDERLINE "\033[4m")

(def (typeln string)
  (display (string-append BOLD OKCYAN))
  (for-each (lambda (c) (display c) (thread-sleep! 0.015)) (string->list string))
  (displayln ENDC ENDC))

(def (run-cli name arguments json-decoder: (json-decoder (lambda (x) x)))
  (def datadir-argument (string-append "-datadir=" (@ client data-directory)))
  (def command ["elements-cli" datadir-argument name arguments ...])
  (def display-command (map (lambda (command) (if (string? command) command (car command))) command))
  (typeln (string-list->string display-command))
  (def actual-command (map (lambda (command) (if (string? command) command (last command))) command))
  (def result (json-decoder (run-process actual-command)))
  (unless (string-empty? result) (displayln result))
  (display "$ ")
  (thread-sleep! 1)
  (string-trim-spaces result))

(def (run-export name value)
  (def command ["set" (string-append name "=" value)]) 
  (typeln (string-list->string command))
  (display "$ ")
  (thread-sleep! 1))

(define-entry-point (no-coin-transaction)
  (help: "Run test scenario for no coin transaction" getopt: [])
  (run-demo (lambda ()
    ; Generate asset
    (display "$ ")
    (def asset (run-cli "issueasset" ["100" "0" "false"]))
    (def asset-hex (hash-get (string->json-object asset) 'asset))
    (run-export "CUSTOM_ASSET" asset-hex)
    (def custom-asset ["$CUSTOM_ASSET" asset-hex])
    
    ; Generate block
    (def address-key (run-cli "getnewaddress" ["demo" "bech32"]))
    (def address ["$ADDRESS" address-key])
    (run-export "ADDRESS" address-key)
    (run-cli "generatetoaddress" ["1" address])
    (run-cli "rescanblockchain" [])

    ; Send asset to new address
    (run-cli "sendtoaddress" [address "1" "null" "null" "null" "null" "null" "unset" "null" custom-asset "null" "null" "\"bitcoin\""])
    (run-cli "generatetoaddress" ["1" address])
    (run-cli "rescanblockchain" [])

    ; Pay fee with new asset
    (def tx (run-cli "sendtoaddress" [address "1" "null" "null" "null" "null" "null" "unset" "null" custom-asset]))
    (run-cli "gettransaction" [tx] 
      json-decoder: (lambda (string) 
        (def json (string->json-object string))
        (hash-put! json 'hex "...")
        (pretty-json json)))
    (thread-sleep! 2)
    "")))

(def (string-list->string list)
  (string-trim-spaces (foldl (lambda (current accumulated) (string-append accumulated " " current)) "" list)))

(current-program "demo")
(set-default-entry-point! 'no-coin-transaction)
(define-multicall-main)
