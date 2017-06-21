
(in-package :common-lisp)

(defpackage :bordeaux-set
  (:use :bordeaux-threads :common-lisp)
  (:shadow
   #:set)
  (:export
   #:set
   #:set-add
   #:set-remove
   #:set-member-p
   #:set-each))
