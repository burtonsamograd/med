(in-package :cl-user)

(defpackage :med-asd
  (:use :cl :asdf))

(in-package :med-asd)

(defsystem :med
  :version "0.1"
  :description "med - Mezzano EDitor"
  :serial t
  :components ((:file "package")
               (:file "line")
               (:file "mark")
               (:file "editor")
               (:file "buffer")
               (:file "point")
               (:file "redisplay")
               (:file "commands")
               (:file "keybindings")
               (:file "minibuffer")
               (:file "repl")
               (:file "main")))
