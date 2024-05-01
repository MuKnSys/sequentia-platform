#!/usr/bin/env gxi
;;; -*- Gerbil -*-
(import
  :clan/building
  :std/sugar)

(def (files)
  [(all-gerbil-modules) ...
   [exe: "rates"]
   "scripts/test"
   "scripts/debug"
   "scripts/demo"])

(init-build-environment!
  name: "sequentia-platform"
  deps: '("clan")
  spec: files)
