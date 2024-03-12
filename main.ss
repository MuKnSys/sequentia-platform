;;; -*- Gerbil -*-
(import :std/error
        :std/sugar
        :std/getopt
        :std/text/json
        ./bitcoin-client
        ./elements-client
        ./sequentia-client)
(export main)

;; build manifest; generated during the build
;; defines version-manifest which you can use for exact versioning
(include "./manifest.ss")

(def (main . args)
  (call-with-getopt gerbil-sequentia-main args
    program: "run"
    help: "A one line description of your program"
    (argument 'type
      help: "node type")
    (argument 'data-directory
      help: "data directory")))

(def* gerbil-sequentia-main
  ((opt)
   (gerbil-sequentia-main/options opt))
  ((cmd opt)
   (gerbil-sequentia-main/command cmd opt)))

;;; Implement this if your CLI doesn't have commands
(def (gerbil-sequentia-main/options opt)
  (run-client (hash-ref opt 'type) (hash-ref opt 'data-directory)))

;;; Implement this if your CLI has commands
(def (gerbil-sequentia-main/command cmd opt)
  (error "Implement me!"))

(def (make-client type)
  (match type
      ('bitcoin (make-BitcoinClient))
      ('elements (make-ElementsClient))
      ('sequentia (make-SequentiaClient))
      (else (error (string-append "Invalid node type: " (symbol->string type))))))

(def (run-client type data-directory)
  (error "Implement me!"))


; createwallet
; createrawtransaction
