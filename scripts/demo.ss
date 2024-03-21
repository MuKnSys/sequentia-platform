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
  {restart-daemon client}
  {initialize-wallet client}
  {rescan-blockchain client})

(def (teardown)
  {stop-daemon client}
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

(def (run-cli name arguments)
    (def datadir-argument (string-append "-datadir=" (@ client data-directory)))
    (def command ["elements-cli" datadir-argument name arguments ...])
    (def display-command (map (lambda (command) (if (string? command) command (car command))) command))
    (displayln "$" BOLD OKCYAN (string-list->string display-command) ENDC ENDC)
    (def actual-command (map (lambda (command) (if (string? command) command (last command))) command))
    (def result (run-process actual-command))
    (displayln result)
    (thread-sleep! 2)
    (string-trim-spaces result))

(def (run-export name value)
    (def command ["set" (string-append name "=" value)]) 
    (displayln "$" BOLD OKCYAN (string-list->string command) ENDC ENDC)
    (displayln)
    (thread-sleep! 2))

(define-entry-point (no-coin-transaction)
  (help: "Run test scenario for no coin transaction" getopt: [])
  (run-demo (lambda ()
    ; Generate asset
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
    (run-cli "sendtoaddress" [address "1" "null" "null" "null" "null" "null" "unset" "null" custom-asset]))))

(def (string-list->string list)
    (foldl (lambda (current accumulated) (string-append accumulated " " current)) "" list))

(current-program "demo")
(set-default-entry-point! 'no-coin-transaction)
(define-multicall-main)
