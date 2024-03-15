(export #t)

(import
  :std/sugar
  :std/misc/alist
  :std/text/json)

; Transactions
(defclass TxInput
  (txid vout sequence))

(defmethod {:json TxInput}
  (lambda (self)
    (list->hash-table
      [(cons "txid" (@ self txid))
       (cons "vout" (@ self vout))
       (cons "sequence" (@ self sequence))])))

(defclass (TxOutput JSON) ())

(defclass (TxAddressOutput TxOutput) (address amount asset))

(defmethod {:json TxAddressOutput}
  (lambda (self)
    (list->hash-table
      [(cons (@ self address) (@ self amount))
       (cons "asset" (@ self asset))])))

(defclass (TxDataOutput TxOutput) (data))

(defclass (TxVDataOutput TxOutput) (vdata))

(defclass (TxBurnOutput TxOutput) (amount))

(defclass (TxFeeOutput TxOutput) (amount))

(defmethod {:json TxFeeOutput}
  (lambda (self)
    (list->hash-table
      [(cons "fee" (@ self amount))])))

(defclass (TxAnyFeeOutput TxFeeOutput) (asset))

(defmethod {:json TxAnyFeeOutput}
  (lambda (self)
    (list->hash-table
      [(cons "fee" (@ self amount))
       (cons "asset" (@ self asset))])))

; Utxo
(defclass (Utxo JSON)
  (txid
   vout
   address
   label
   scriptPubKey
   amount
   asset
   assetcommitment
   amountblinder
   assetblinder
   confirmations
;   redeemScript
;  witnessScript
   spendable
   solvable
 ;  reused
   desc
   safe))

; Issuance
(defclass (Issuance JSON)
  (asset_amount
   asset_address
   token_amount
   token_address
   blind
   contract_hash))
