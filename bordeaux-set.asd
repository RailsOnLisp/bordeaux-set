
(in-package :common-lisp-user)

(defpackage :bordeaux-set.system
  (:use :common-lisp :asdf))

(in-package :bordeaux-set.system)

(defsystem "bordeaux-set"
  :name "bordeaux-set"
  :author "Thomas de Grivel <thoxdg@gmail.com>"
  :version "0.1"
  :description "Re-entrant set using bordeaux-threads."
  :depends-on ("bordeaux-threads")
  :components
  ((:file "package")
   (:file "bordeaux-set" :depends-on ("package"))))
