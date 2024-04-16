;; https://www.blockchain.com/explorer/api/exchange_rates_api
;; https://blockchain.info/ticker

;; https://site.financialmodelingprep.com/developer/docs/bitcoin-price-free-api

;; https://coinlayer.com/documentation

(import
  (group-in :std format sort sugar)
  :std/debug/DBG
  (group-in :std/misc hash path number ports repr)
  (group-in :std/net request uri)
  (group-in :std/srfi |1|)
  (group-in :std/text json)
  (group-in :clan config json timestamp debug))

;; Compute the median of a list of reals
;; Real <- (Listof Real)
(def (median l (default 0))
  (def s (sort l <))
  (def n (length l))
  (def i (half (1- n)))
  (def j (half n))
  (cond
   ((zero? n) default)
   ((= i j) (list-ref s i))
   (else (* .5 (+ (list-ref s i) (list-ref s j))))))

;; (Table (List Function Function) <- String)
(def price-oracles (hash))

;; <- String Function Function
(def (register-price-oracle name get-quote get-rate)
  (hash-put! price-oracles name [get-quote get-rate]))

;; (Table HashTable <- String)
(def test-rates-services-config
  (hash ("coinmarketcap" (hash ("host" "sandbox-api.coinmarketcap.com")
                               ("key" "b54bcf4d-1bca-4e8e-9a24-22ff2c3d462c")
                               ("refractory_period" 30)))))

;; (Table HashTable <- String)
(def test-rates-assets-config
  (hash ("BTC" (hash ("nAsset" "0000000000000000000000000000000000000000000000000000000000000001")
                     ("usual_decimals" 8)
                     ("on_chain_scale" 1e8)
                     ("oracles" (hash ("coinmarketcap" 2))))) ;; in test, symbol changes every time
        ("ETH" (hash ("nAsset" "0000000000000000000000000000000000000000000000000000000000000002")
                     ("usual_decimals" 18)
                     ("on_chain_scale" 1e8)
                     ("oracles" (hash ("coinmarketcap" 4)))))))


;; (Parameter (Table HashTable <- String))
(def *rates-services-config*
  (make-parameter (hash)))

;; (Parameter (Table HashTable <- String))
(def *rates-assets-config*
  (make-parameter (hash)))

;; <-
(def (use-test-rates-config)
  (*rates-services-config* test-rates-services-config)
  (*rates-assets-config* test-rates-assets-config))

;; JSON <- String
(def (read-config file)
   (try (read-file-json file)
        (catch (_) (error "Failed to read JSON config file" file))))

;; <-
(def (use-prod-rates-config)
  (*rates-services-config*
   (read-config (xdg-config-home "sequentia/rates-services-config.json")))
  (*rates-assets-config*
   (read-config (xdg-config-home "sequentia/rates-assets-config.json"))))

(defrule (defprice-oracle name ((config) body1 ...) ((quote-json path) body2 ...))
  (with-id defprice-oracle ((get-quote 'get- #'name '-quote)
                            (get-rate 'get- #'name '-rate))
    (begin
      (def (get-quote (config (hash-ref (*rates-services-config*) (as-string 'name)))) body1 ...)
      (def (get-rate quote-json path) body2 ...)
      (register-price-oracle (as-string 'name) get-quote get-rate))))

;; For each oracle, the timestamp of last query attempt, timestamp of data, data (#f if none)
;; (Table (List TAI TAI JSON) <- String)
(def oracle-prices (hash))

;; Given an oracle, return a list of the timestamp of last query attempt,
;; timestamp of data, and data (#f if none)
;; (List TAI TAI JSON) <- String
(def (get-oracle-data oracle services-config: (services-config *rates-services-config*))
  (let/cc k
    (def (return x)
      (hash-put! oracle-prices oracle x)
      (k x))
    (def config (hash-ref services-config oracle))
    (def refractory-period (hash-ref config "refractory_period"))
    (def (refresh)
      (match (hash-ref price-oracles oracle)
        ([get-quote _]
         (let* ((new-quote (get-quote config))
                (stamp (current-tai-timestamp)))
           (return [stamp stamp new-quote])))))
    (def previous (hash-get oracle-prices oracle))
    (match previous
      ([attempt-tai data-tai quote-data]
       (when (< (current-tai-timestamp)
                (+ attempt-tai refractory-period))
         (return previous))
       (try
        (refresh)
        (catch (_) (return [(current-tai-timestamp) data-tai quote-data]))))
      (#f
       (try
        (refresh)
        (catch (_)
          (let ((stamp (current-tai-timestamp)))
            (return [stamp stamp #f]))))))))


(def (get-rate/oracle-path
      oracle path
      services-config: (services-config (*rates-services-config*)))
  (def service-config (hash-ref services-config oracle))
  (def data (third (get-oracle-data oracle services-config: services-config)))
  (match (hash-ref price-oracles oracle)
    ([_ get-rate]
     (get-rate data path))))

(def (get-rates
      assets-config: (assets-config (*rates-assets-config*))
      services-config: (services-config (*rates-services-config*)))
  (hash-value-map
   assets-config
   (lambda (asset)
     (median (filter identity
                     (map (match <>
                            ([oracle . path]
                             (get-rate/oracle-path
                              oracle path services-config: services-config)))
                          (hash->list (hash-ref asset "oracles"))))))))

;; https://coinmarketcap.com/api/documentation/v1/#section/Quick-Start-Guide
;; NB: Free has 10K calls per month, so safe once every 2 hours,
;; or once a minute for about 2h45. https://pro.coinmarketcap.com/api/pricing
;; The $30/mo plan has 110K calls per month, enough for twice a minute
(def (coinmarketcap-select data selector)
  (cond
   ((string? selector) (find (lambda (x) (equal? (hash-ref x "symbol") selector)) data))
   ((fixnum? selector) (list-ref data selector))
   (else (error "bad selector" selector))))
(defprice-oracle coinmarketcap
  ((config)
   (let ((host (hash-ref config "host"))
         (key (hash-ref config "key")))
     (bytes->json-object
      (request-content
       (http-get (query-string (format "https://~a/v1/cryptocurrency/listings/latest" host)
                               start: 1 limit: 5000 convert: 'USD)
                 headers: [["X-CMC_PRO_API_KEY" . key]
                           ["Accept" . "application/json"]])))))
  ((quote-json selector)
   (let* ((data (hash-ref quote-json "data"))
          (entry (coinmarketcap-select data selector)))
     (hash-ref (hash-ref (hash-ref entry "quote") "USD") "price"))))

#||#
;;(trace! get-rates get-rate/oracle-path get-oracle-data get-coinmarketcap-quote get-coinmarketcap-rate)
(def (pj x) (pretty-json x #t)) ;; lisp-style?: #t))
(json-symbolic-keys #f)
(json-sort-keys #t)
;;(use-test-rates-config)
(use-prod-rates-config)

;;(apropos "coinmarketcap")
;;(pj (*rates-services-config*))
;;(pj (*rates-assets-config*))
;;(def q (get-coinmarketcap-quote))
;;(pj q)
;;(writeln (map (cut hash-ref <> "symbol") (hash-ref q "data"))) ;; changes every time
(pj (get-rates))
