(ql:quickload 'quickutil)

(qtlc:save-utils-as
  "quickutils.lisp"
  :utilities '(:map-product
               :hash-table-key-exists-p
               :hash-table-values
               :symb
               :ensure-keyword)
  :package "BEAST.QUICKUTILS")
