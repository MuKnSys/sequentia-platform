#!/usr/bin/env gxi

(import
  :std/cli/getopt
  :std/cli/multicall
  :std/misc/process
  :std/sugar
  :mukn/sequentia/client
  :mukn/sequentia/types
  :mukn/sequentia/test-client)

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
