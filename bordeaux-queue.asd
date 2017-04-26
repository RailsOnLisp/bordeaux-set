
(in-package :common-lisp-user)

(defpackage :bordeaux-queue.system
  (:use :common-lisp :asdf))

(in-package :bordeaux-queue.system)

(defsystem "bordeaux-queue"
  :name "bordeaux-queue"
  :author "Thomas de Grivel <thoxdg@gmail.com>"
  :version "0.1"
  :description "Re-entrant queue using bordeaux-threads."
  :depends-on ("bordeaux-threads")
  :components
  ((:file "package")
   (:file "bordeaux-queue" :depends-on ("package"))))
