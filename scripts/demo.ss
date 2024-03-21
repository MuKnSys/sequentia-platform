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
    (displayln "$" BOLD OKCYAN (string-list->string command) ENDC ENDC)
    (def result (run-process command))
    (thread-sleep! 1)
    (displayln result)
    (thread-sleep! 2)
    (string-trim-spaces result))

(define-entry-point (no-coin-transaction)
  (help: "Run test scenario for no coin transaction" getopt: [])
  (run-demo (lambda ()
    ; Generate asset
    (def asset (run-cli "issueasset" ["100" "0" "false"]))
    (def asset-hex (hash-get (string->json-object asset) 'asset))
    (run-cli "rescanblockchain" [])
    
    ; Generate block
    (def funding-address (run-cli "getnewaddress" ["demo" "bech32"]))
    (run-cli "generatetoaddress" ["1" funding-address])
    (run-cli "rescanblockchain" [])

    ; Send asset to new address
    (run-cli "sendtoaddress" [funding-address "1" "null" "null" "null" "null" "null" "unset" "null" asset-hex "null" "null" "\"bitcoin\""])
    (run-cli "generatetoaddress" ["1" funding-address])
    (run-cli "rescanblockchain" [])

    ; Pay fee with new asset
    (run-cli "sendtoaddress" [funding-address "1" "null" "null" "null" "null" "null" "unset" "null" asset-hex]))))

(def (string-list->string list)
    (foldl (lambda (current accumulated) (string-append accumulated " " current)) "" list))

(current-program "demo")
(set-default-entry-point! 'no-coin-transaction)
(define-multicall-main)
