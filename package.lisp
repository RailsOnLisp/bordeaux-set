
(in-package :common-lisp)

(defpackage :bordeaux-queue
  (:use :bordeaux-threads :common-lisp)
  (:export
   #:queue
   #:enqueue
   #:dequeue
   #:dequeue-all))
