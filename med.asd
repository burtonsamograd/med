(in-package :cl-user)

(defpackage :med-asd
  (:use :cl :asdf))

(in-package :med-asd)

(defsystem :med
  :version "0.1"
  :description "med - Mezzano EDitor"
  :serial t
  :components ((:file "all")))
