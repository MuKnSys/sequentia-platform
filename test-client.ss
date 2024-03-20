(import
  ./sequentia-client)
(export #t)

(def client (make-SequentiaClient
    data-directory: "./data"
    options: ["-logsourcelocations"]
    host: "127.0.0.1"
    port: 18884
    username: "user1"
    password: "password1"
    log-file: "../debug.log"))
