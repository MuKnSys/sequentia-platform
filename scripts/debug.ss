#!/usr/bin/env gxi

(import
  :std/assert
  :std/cli/getopt
  :std/cli/multicall
  :std/misc/process
  :std/sugar
  :mukn/sequentia/sequentia-client
  :mukn/sequentia/types)

(def client (make-SequentiaClient
    data-directory: "./data"
    options: ["-logsourcelocations"]
    host: "127.0.0.1"
    port: 18884
    username: "user1"
    password: "password1"
    log-file: "../debug.log"))

; Daemon management
(define-entry-point (start)
  (help: "Start an elements daemon" getopt: [])
  {start-daemon client})

(define-entry-point (stop)
  (help: "Stop elements daemon" getopt: [])
  {stop-daemon client})

(define-entry-point (restart)
  (help: "Restart elements daemon" getopt: [])
  {restart-daemon client})

; Debugging chain state
(define-entry-point (dump-asset-labels)
  (help: "Dump asset labels" getopt: [])
  {dump-asset-labels client})

(define-entry-point (get-balances)
  (help: "Get wallet balances" getopt: [])
  {get-balances client})

(define-entry-point (list-unspent)
  (help: "List UTXOs" getopt: [])
  {list-unspent client})

(define-entry-point (rescan-blockchain)
  (help: "Rescan blockchain" getopt: [])
  {rescan-blockchain client})

(define-entry-point (get-transaction tx-id)
  (help: "Get transaction info" getopt: [(argument 'tx-id help: "transaction id")])
  {get-transaction client tx-id})

(current-program "debug")
(set-default-entry-point! 'start)
(define-multicall-main)
