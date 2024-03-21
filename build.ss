#!/usr/bin/env gxi
;;; -*- Gerbil -*-
(import
  :clan/building
  :std/sugar)

(def (files)
  [(all-gerbil-modules) ...
   "scripts/test"
   "scripts/debug"
   "scripts/demo"])

(init-build-environment!
  name: "Gerbil-sequentia"
  deps: '("clan" "clan/crypto")
  spec: files)
