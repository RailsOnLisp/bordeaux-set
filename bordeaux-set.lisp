
(in-package :bordeaux-set)

(deftype fixnum+ (&optional (low 0))
  `(integer ,low ,most-positive-fixnum))

(defclass set ()
  ((hash-table :initarg :hash-table
	       :initform (make-hash-table)
	       :reader set-hash-table
	       :type hash-table)
   (size :initform 0
	 :accessor set-size
	 :type fixnum+)
   (lock :initform (make-lock "set")
	 :reader set-lock)))

(defgeneric set-add (set item))
(defgeneric set-remove (set item))
(defgeneric set-member-p (set item))
(defgeneric set-empty-p (set))
(defgeneric do-set (fn set))

(defmethod set-add ((set set) item)
  (with-lock-held ((set-lock set))
    (setf (gethash item (set-hash-table set)) t)
    (incf (set-size set))))

(defmethod set-remove ((set set) item)
  (with-lock-held ((set-lock set))
    (remhash item (set-hash-table set))
    (decf (set-size set))))

(defmethod set-member-p ((set set) item)
  (with-lock-held ((set-lock set))
    (gethash item (set-hash-table set))))

(defmethod set-empty-p ((set set))
  (= 0 (set-size set)))

(defmethod set-each ((fn function) (set set))
  (with-lock-held ((set-lock set))
    (maphash (lambda (key value)
	       (declare (ignore value))
	       (funcall fn key))
	     (set-hash-table set))))
