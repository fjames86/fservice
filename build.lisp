
(ql:quickload "cffi")
(ql:quickload "bordeaux-threads")
(ql:quickload "pounds")
(ql:quickload "swank")
(ql:quickload "fservice") 
(load "example.lisp")
(swank-loader:init)

(swank:swank-require '(SWANK-IO-PACKAGE::SWANK-TRACE-DIALOG
SWANK-IO-PACKAGE::SWANK-PACKAGE-FU
SWANK-IO-PACKAGE::SWANK-PRESENTATIONS
SWANK-IO-PACKAGE::SWANK-FUZZY
SWANK-IO-PACKAGE::SWANK-FANCY-INSPECTOR
SWANK-IO-PACKAGE::SWANK-C-P-C
SWANK-IO-PACKAGE::SWANK-ARGLISTS
SWANK-IO-PACKAGE::SWANK-REPL))

(sb-ext:save-lisp-and-die "service.exe" :toplevel #'fservice.example::main :executable t)

