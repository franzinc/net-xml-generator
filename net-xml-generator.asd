(in-package :asdf)

(defsystem :net-xml-generator
  :name "XML Generator"
  :author "Franz Inc"
  :version "0.1"
  :serial t
  :components 
  ((:static-file "net-xml-generator.asd")
   (:file "net-xml-generator")))
