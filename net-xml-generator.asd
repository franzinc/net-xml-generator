(in-package :cl-user)

(defpackage :net-xml-generator.asdf
  (:use :cl :asdf))
(in-package :net-xml-generator.asdf)

(unless (find-class 'cl-file nil)
  (defclass asdf::cl-file (asdf:cl-source-file) ())
  (defmethod asdf:source-file-type ((c asdf::cl-file)
				    (s asdf:module))
    "cl"))

(defsystem :net-xml-generator
    :license "LLGPL"
    :author "Steve Haflich <smh@franz.com>"
    :depends-on ()
    :components ((:cl-file "net-xml-generator")))
