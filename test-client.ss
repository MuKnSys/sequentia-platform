(import
  ./sequentia-client)
(export #t)

(def client (make-SequentiaClient
    host: "127.0.0.1"
    port: 18884
    username: "user1"
    password: "password1"
    data-directory: "./data"
    log-file: "../debug.log"
    exchange-rates-json-file: "../exchangerates.json"
    daemon-options: ["-logsourcelocations" "-con_any_asset_fees=1"]))
