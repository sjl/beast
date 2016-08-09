(ql:quickload 'quickutil)

(qtlc:save-utils-as
  "quickutils.lisp"
  :utilities '(:map-product
               :hash-table-key-exists-p
               :hash-table-values
               :with-gensyms
               :symb
               :ensure-keyword
               :ensure-list)
  :package "BEAST.QUICKUTILS")
