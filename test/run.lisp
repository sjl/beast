#+ecl (setf compiler:*user-cc-flags* "-Wno-shift-negative-value")

(ql:quickload 'beast :silent t)
(asdf:test-system 'beast)
(quit)
