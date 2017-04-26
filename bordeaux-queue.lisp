
(in-package :bordeaux-queue)

(deftype fixnum+ (&optional (low 0))
  `(integer ,low ,most-positive-fixnum))

(deftype fixnum* (&optional (low 1))
  `(integer ,low ,most-positive-fixnum))

(defclass queue ()
  ((vector :initarg :vector
	   :reader queue-vector
	   :type simple-vector)
   (read-index :initform 0
	       :accessor queue-read-index
	       :type fixnum+)
   (write-index :initform 0
		:accessor queue-write-index
		:type fixnum+)
   (length :initform 0
	   :accessor queue-length
	   :type fixnum+)
   (lock :initform (make-lock 'queue)
	 :reader queue-lock)))

(defclass queue-blocking-write (queue)
  ((blocking-write-cv :initform (make-condition-variable
				 :name "queue blocking write")
		      :reader queue-blocking-write-cv)))

(defgeneric enqueue-full (queue))

(defgeneric enqueue (queue item &optional blocking))

(defgeneric on-dequeue (queue))
(defgeneric dequeue (queue))
(defgeneric dequeue-all (queue))

;;  Initialization

(defun make-queue-vector (size &optional (element-type 't) initial-element)
  (declare (type fixnum* size))
  (assert (typep initial-element element-type))
  (make-array `(,size)
	      :element-type element-type
	      :initial-element initial-element))

(defmethod initialize-instance ((q queue) &rest initargs
				&key size (element-type 't)
				  initial-element &allow-other-keys)
  (let ((vector (make-queue-vector size element-type initial-element)))
    (apply #'call-next-method q (list* :vector vector initargs))))

(defun make-queue (size)
  (make-instance 'queue :size size))

;;  Enqueue

(defmethod enqueue-full ((q queue))
  nil)

(defmethod enqueue-full ((q queue-blocking-write))
  (with-accessors ((lock queue-lock)
		   (blocking-write-cv queue-blocking-write-cv)) q
    (condition-wait blocking-write-cv lock)))

(defmethod enqueue ((q queue) item &optional blocking)
  (assert (typep item (array-element-type (queue-vector q))))
  (with-accessors ((vector queue-vector)
		   (write-index queue-write-index)
		   (length queue-length)
		   (lock queue-lock)) q
    (with-lock-held (lock)
      (labels ((fetch ()
		 (let ((vector-length (length vector))
		       (len length))
		   (cond ((= len vector-length)
			  (when blocking
			    (enqueue-full q)
			    (fetch)))
			 ((< len vector-length)
			  (setf write-index (mod (1+ write-index) vector-length)
				length (1+ len))
			  t)
			 (t
			  (error "Invalid queue length"))))))
	(fetch)))))

;;  Dequeue

(defmethod on-dequeue ((q queue))
  nil)

(defmethod on-dequeue ((q queue-blocking-write))
  (condition-notify (queue-blocking-write-cv q)))

(defmethod dequeue ((q queue))
  (with-accessors ((vector queue-vector)
		   (read-index queue-read-index)
		   (length queue-length)
		   (lock queue-lock)) q
    (with-lock-held (lock)
      (let ((vector-length (length vector))
	    (len length))
	(cond ((= 0 len)
	       (values nil nil))
	      ((< 0 len)
	       (let ((item (aref vector read-index)))
		 (setf read-index (mod (1+ read-index) vector-length)
		       length (1- len))
		 (on-dequeue q)
		 (values item t)))
	      (t
	       (error "Invalid queue length")))))))
