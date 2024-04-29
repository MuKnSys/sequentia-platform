#!/usr/bin/env gxi
;;;;; Rates server

;; BEWARE: >>>> CONFIGURATION <<<<
;; copy the sample rates-assets-config.json and rates-services-config.json
;; from data/ to ~/.config/sequentia/ and edit rates-services-config.json to use your keys,
;; as gotten from:
;; https://www.coinapi.io/get-free-api-key?email=xxx
;; https://coinlayer.com/product (choose the free plan, still need a credit card)
;; https://coinmarketcap.com/api/pricing/ (choose the free plan)
;; https://site.financialmodelingprep.com/developer/docs/pricing (choose the basic plan)


;;; Imports

(import
  (group-in :std sort sugar)
  (group-in :std/misc hash path number)
  (group-in :std/net request uri)
  (group-in :std/srfi |1|)
  (group-in :std/text json)
  (group-in :clan config files json timestamp))

(import ;; for development and debugging
  (group-in :std/debug DBG)
  (group-in :std/misc ports repr)
  (group-in :clan debug))


;;; General utility functions, to move somewhere else

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

;; Access successive subnodes in a JSON object
(defrules hash-ref* ()
  ((h x) x)
  ((h x y . z) (h (hash-ref x y) . z)))

;; List to hash-table given
;; HashTable <- List
(def (list->hash-table/by-symbol lst (field "symbol"))
  (list->hash-table (map (lambda (x) (cons (hash-ref x field) x)) lst)))

;; Access a quote of given symbol from a list
;; HashTable <- (Listof HashTable) (Or String Fixnum)
(def (symbol-select data selector (field "symbol"))
  (cond
   ((string? selector) (find (lambda (x) (equal? (hash-ref x field) selector)) data))
   ((fixnum? selector) (list-ref data selector))
   (else (error "bad selector" selector))))

;; JSON <- String
(def (read-json-config file)
   (try (read-file-json file)
        (catch (e) 
          (display-exception e)
          (error "Failed to read JSON config file" file))))


;;; A registry of named oracles and their access methods

;; (Table (List Function Function) <- String)
(def price-oracles (hash))

;; <- String Function Function
(def (register-price-oracle name get-quote get-rate)
  (hash-put! price-oracles name [get-quote get-rate]))


;;; Configuration

;; (Parameter (Table HashTable <- String))
(def *rates-services-config*
  (make-parameter (hash)))

;; (Parameter (Table HashTable <- String))
(def *rates-assets-config*
  (make-parameter (hash)))

;; <-
(def (read-rates-config)
  (*rates-services-config*
   (read-json-config (xdg-config-home "sequentia/rates-services-config.json")))
  (*rates-assets-config*
   (read-json-config (xdg-config-home "sequentia/rates-assets-config.json"))))


;;; Cached prices from oracles

;; For each oracle, the timestamp of last query attempt, timestamp of data, data (#f if none)
;; (Table (List TAI TAI JSON) <- String)
(def oracle-prices (hash))

;; String <-
(def (oracle-prices-cache-path)
  (xdg-cache-home "sequentia/oracle-prices.json"))

;; <-
(def (load-oracle-prices-cache)
  (set! oracle-prices
    (try
     (read-file-json (oracle-prices-cache-path))
     (catch (_) (hash)))))

;; <-
(def (save-oracle-prices-cache)
  (create-directory* (xdg-cache-home "sequentia"))
  (clobber-file (oracle-prices-cache-path)
                (cut write-json oracle-prices <>)))

;; TODO: save pristine data as obtained from source, just compressed, in a sqlite database.


;;; Getting rates from lazily-updated cache of oracles

;; Given an oracle, return a list of the timestamp of last query attempt,
;; timestamp of data, and data (#f if none)
;; (List TAI TAI JSON) <- String
(def (get-oracle-data oracle services-config: (services-config (*rates-services-config*)))
  (let/cc k
    (def (return x)
      (unless (equal? x (hash-get oracle-prices oracle))
        (hash-put! oracle-prices oracle x)
        (save-oracle-prices-cache))
      (k x))
    (def config (hash-ref services-config oracle))
    (def refractory-period (hash-ref config "refractory_period"))
    (def (refresh)
      (match (hash-ref price-oracles oracle)
        ([get-quote _]
         (let* ((new-quote (get-quote config))
                (stamp (current-tai-timestamp))
                (result [stamp stamp new-quote]))
           (return result)))))
    (def previous (hash-get oracle-prices oracle))
    (match previous
      ([attempt-tai data-tai quote-data]
       (when (and attempt-tai
                  (< (current-tai-timestamp)
                     (+ attempt-tai (* refractory-period one-second))))
         (return previous))
       (try
        (refresh)
        (catch (_) (return [(current-tai-timestamp) data-tai quote-data]))))
      (_
       (try
        (refresh)
        (catch (_) (return [#f #f #f])))))))

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
     (hash-key-value-map
      (lambda (oracle path)
        (cons oracle (get-rate/oracle-path oracle path services-config: services-config)))
      (hash-ref asset "oracles")))))

(def (median<-rates rates)
  (median (filter identity (hash-values rates)) #f))

(def (get-median-rates
      assets-config: (assets-config (*rates-assets-config*))
      services-config: (services-config (*rates-services-config*)))
  (hash-value-map
   (get-rates assets-config: assets-config
              services-config: services-config)
   median<-rates))


;;; The access methods

(defrule (defprice-oracle name ((config) body1 ...) ((quote-json path) body2 ...))
  (with-id defprice-oracle ((get-quote 'get- #'name '-quote)
                            (get-rate 'get- #'name '-rate))
    (begin
      (def (get-quote (config (hash-ref (*rates-services-config*) (as-string 'name)))) body1 ...)
      (def (get-rate quote-json path) body2 ...)
      (register-price-oracle (as-string 'name) get-quote get-rate))))

;; Blockchain.info (for Bitcoin price only)
;; https://www.blockchain.com/explorer/api/exchange_rates_api
;; https://blockchain.info/ticker
(defprice-oracle blockchain.info
  ((config)
   (bytes->json-object
    (request-content
     (http-get "https://blockchain.info/ticker"))))
  ((quote-json selector)
   (unless (equal? selector "BTC")
     (error "bad selector" selector))
   (hash-ref* quote-json "USD" "last")))

;; CEX.io
;; https://cex.io/rest-api#ticker
(defprice-oracle cex.io
  ((config)
   (bytes->json-object
    (request-content
     (http-get "https://cex.io/api/tickers/USD"))))
  ((quote-json selector)
   (hash-ref (symbol-select (hash-ref quote-json "data") selector "pair") "last")))

;; Coinapi.io
;; https://docs.coinapi.io/
(defprice-oracle coinapi.io
  ((config)
   (let ((key (hash-ref config "key")))
     (bytes->json-object
      (request-content
       (http-get "https://rest.coinapi.io/v1/exchangerate/USD?invert=true"
                 headers: [["X-CoinAPI-Key" . key]])))))
  ((quote-json selector)
   (hash-ref (symbol-select (hash-ref quote-json "rates") selector "asset_id_quote") "rate")))


;; Coinlayer.com
;; https://coinlayer.com/documentation
;; Free API key has only 100 queries per month, so set refractory period to 28800 (8 hours)
;; Also, free API can only use http, not https. There is no test server.
;; See how many queries you have left at: https://coinlayer.com/dashboard
(defprice-oracle coinlayer.com
  ((config)
   (let ((protocol (hash-ref config "protocol"))
         (key (hash-ref config "key")))
     (bytes->json-object
      (request-content
       (http-get (query-string (as-string protocol "://api.coinlayer.com/live")
                               access_key: key))))))
  ((quote-json selector)
   (hash-ref* quote-json "rates" selector)))

;; Coinmarketcap.com
;; https://coinmarketcap.com/api/documentation/v1/#section/Quick-Start-Guide
;; NB: Free has 10K calls per month, so safe once every 2 hours,
;; or once a minute for about 2h45. https://pro.coinmarketcap.com/api/pricing
;; The $30/mo plan has 110K calls per month, enough for twice a minute
;; Test server serves garbage, albeit in the right format.
(defprice-oracle coinmarketcap.com
  ((config)
   (let ((key (hash-ref config "key")))
     (bytes->json-object
      (request-content
       (http-get (query-string "https://pro-api.coinmarketcap.com/v1/cryptocurrency/listings/latest"
                               start: 1 limit: 5000 convert: 'USD)
                 headers: [["X-CMC_PRO_API_KEY" . key]
                           ["Accept" . "application/json"]])))))
  ((quote-json selector)
   (hash-ref* (symbol-select (hash-ref quote-json "data") selector) "quote" "USD" "price")))

;; Financialmodelingprep.com
;; https://site.financialmodelingprep.com/developer/docs/bitcoin-price-free-api
;; free is 250 calls/day, with 2 things, every 6 minutes
(defprice-oracle financialmodelingprep.com
  ((config)
   (let* ((key (hash-ref config "key"))
          (pairs (hash-ref config "asset_pairs"))
          (url (as-string "https://financialmodelingprep.com/api/v3/quote/" pairs)))
     (list->hash-table/by-symbol
      (bytes->json-object
       (request-content
        (http-get (query-string url apikey: key)))))))
  ((quote-json selector)
   (hash-ref* quote-json selector "price")))

;; Polygon.io
;; https://polygon.io/docs/stocks/get_v2_last_nbbo__stocksticker
;; Access requires $80/mo subscription... not for now
(defprice-oracle polygon.io
  ((config)
   (let ((key (hash-ref config "key"))
         (tickers (hash-ref config "tickers")))
     (list->hash-table
      (map (lambda (ticker)
             (cons ticker
                   (bytes->json-object
                    (request-content
                     (http-get (query-string ;; only previous close fro
                                (as-string "https://api.polygon.io/v2/aggs/ticker/" ticker "/prev")
                                apiKey: key))))))
           tickers))))
  ((quote-json selector)
   ;; This is very imprecise, only close from last day
   (hash-ref (symbol-select (hash-ref* quote-json selector "results") selector "T") "c")))

;; TODO: add more data sources?
;; https://medium.com/coinmonks/free-stock-apis-de8f13619911
;; https://www.alphavantage.co/documentation/
;; https://finnhub.io/

;;; TODO: Connecting to a sequentia node


;;; Testing the above, for now, until we have a complete thing
#||#

(def (pj x) (pretty-json x (current-output-port))) ;; lisp-style?: #t))

(def (main)
  (json-symbolic-keys #f) ;; (read-json-key-as-symbol? #f)
  (json-sort-keys #t) ;; (write-json-sort-keys? #t)
  (read-rates-config)
  (load-oracle-prices-cache)

  ;;(writeln (sort (hash-keys price-oracles) string<?))
  ;;(apropos "coinlayer")
  ;;(trace! get-rates get-rate/oracle-path get-oracle-data get-coinmarketcap-quote get-coinmarketcap-rate)
  ;;(pj (get-coinlayer-quote))
  ;;(pj (get-oracle-data "coinlayer.com"))
  ;;(pj (get-financialmodelingprep.com-quote))
  ;;(pj (get-cex.io-quote))
  ;;(pj (get-oracle-data "financialmodelingprep.com"))
  ;;(pj (get-oracle-data "financialmodelingprep.com"))
  ;;(pj oracle-prices)
  ;;(pj (*rates-services-config*))
  ;;(pj (*rates-assets-config*))

  ;;(let (q (get-coinmarketcap.com-quote))
  ;;  (pj q)
  ;;  (writeln (map (cut hash-ref <> "symbol") (hash-ref q "data")))
  ;;  (writeln (length (hash-ref q "data"))))

  ;;(let (q (get-coinlayer.com-quote))
  ;;  (pj q)
  ;;  (writeln (sort (hash-keys (hash-ref q "rates")) string<?))
  ;;  (writeln (hash-length (hash-ref q2 "rates"))))

  ;;(pj (get-coinapi.io-quote))
  ;;(writeln (length (hash-ref (get-coinapi.io-quote) "rates"))) ; => 4999

  ;;(pj (get-polygon.io-quote))

  (pj (get-rates))
  ;;(pj (get-median-rates))

  (save-oracle-prices-cache)
  (displayln "See price cache at: " (oracle-prices-cache-path))
  (void))
