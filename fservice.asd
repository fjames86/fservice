;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

(asdf:defsystem :fservice
  :name "fservice"
  :author "Frank James <frank.a.james@gmail.com>"
  :description "Common Lisp Windows service"
  :license "MIT"
  :components
  ((:file "service"))
  :depends-on (:cffi :bordeaux-threads))


