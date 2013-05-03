
(defpackage #:weblocks-d-s-r
  (:use :cl  :weblocks)
  (:documentation
   "A driver for weblocks backend store API that connects to de.setf.resource."))

(in-package :weblocks-d-s-r)

(export '(order-by-expression range-to-offset range-to-limit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Initialization/finalization ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod open-store ((store-type (eql :d-s-r)) &key (mediator-type (eql :wilbur-n3-file)) pathname)
  (with-open-file (in pathname :direction :input)
    (rdf:load-repository-as (rdf:mediator-repository (rdf:wilbur-mediator)) in mime:application/n3))
  (setf *default-store* (rdf:wilbur-mediator)))

#+l(defmethod close-store ((store database))
  (when (eq *default-store* store)
    (setf *default-store* nil))
  (disconnect :database store))

#+l(defmethod clean-store ((store database))
  (dolist (seq (list-sequences :database store))
    (drop-sequence seq :database store))
  (dolist (table (list-tables :database store))
    (delete-records :from table :database store)))

;;;;;;;;;;;;;;;;;;;;
;;; Transactions ;;;
;;;;;;;;;;;;;;;;;;;;
(defun transactions-warning ()
  (warn "Tranactions not implemented yet"))

(defmethod begin-transaction ((store rdf:repository-mediator))
  (transactions-warning))

(defmethod commit-transaction ((store rdf:repository-mediator))
  (transactions-warning))

(defmethod rollback-transaction ((store rdf:repository-mediator))
  (transactions-warning))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Creating and deleting persistent objects ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#+l(defmethod persist-object ((store database) object &key)
  ;; Note, we persist new objects in three steps, this should be
  ;; optimized into a single query later
  (let* ((class-name (class-name (class-of object)))
	 (current-id (object-id object))
	 (sequence-name (intern (concatenate 'string
					     (symbol-name class-name)
					     (symbol-name '#:-seq))
				(symbol-package class-name))))
    (unless current-id
      ;; Create sequence if necessary
      (unless (sequence-exists-p sequence-name :database store :owner :all)
	(create-sequence sequence-name :database store))
      ;; Set the id to next sequence number
      (setf (object-id object)
	    (sequence-next sequence-name :database store)))
    ;; Persist object
    (let (success)
      (unwind-protect
	   (progn
	     (update-records-from-instance object :database store)
	     (setf success t)
             object)
	(when (and (not success)
		   (null current-id))
	  (setf (object-id object) nil))))))

#+l(defmethod delete-persistent-object ((store database) object)
  (delete-instance-records object))

#+l(defmethod delete-persistent-object-by-id ((store database) class-name object-id)
  (delete-records :from class-name
		  :where [= (sql-expression :attribute (class-id-slot-name class-name))
		            object-id]
		  :database store)
  )

;;;;;;;;;;;;;
;;; Utils ;;;
;;;;;;;;;;;;;
#+l(defun slot-db-info (class slot-name)
  "Returns clsql db-info structure."
  )

#+l(defun class-order-by-join-classes (class-name order-by)
  "Returns a list of class names that need to be selected to find
instances of 'class-name' and order them with 'order-by'."
  (flet ((slot-join-class (class slot-name)
	   (gethash :join-class (slot-db-info class slot-name))))
    (loop
       for slot in (cons nil (drop-last (ensure-list (car order-by))))
       for class = class-name then (slot-join-class class slot)
       collect class)))

#+l(defun class-order-by-join-where (class-name order-by)
  "Returns a 'where' expression that joins classes determined by
'class-order-by-join-classes' in order to find instances of
'class-name' and order them with 'order-by'."
  (when (and (car order-by)
	     (listp (car order-by))
	     (second (car order-by)))
    (flet ((slot-home-key (class slot-name)
	     (gethash :home-key (slot-db-info class slot-name)))
	   (slot-foreign-key (class slot-name)
	     (gethash :foreign-key (slot-db-info class slot-name))))
      (apply #'sql-operation 'and
	     (remove nil
		     (maplist (lambda (classes slots)
				(let ((c1 (first classes))
				      (c2 (second classes))
				      (slot (car slots)))
				  (when (and c1 c2)
				    (sql-operation '=
						   (sql-operation 'slot-value c1
								  (slot-home-key c1 slot))
						   (sql-operation 'slot-value c2
								  (slot-foreign-key c1 slot))))))
			      (class-order-by-join-classes class-name order-by)
			      (drop-last (car order-by))))))))

#+l(defun order-by-expression (class-name order-by)
  "Converts the 'order-by' argument to a SQL expression."
  (let ((order-path (car order-by)))
    (when order-by
      (list (list (if (listp order-path)
		      (sql-operation 'slot-value
				     (last-element (class-order-by-join-classes class-name order-by))
				     (last-element order-path))
		      (sql-expression :attribute order-path))
		  (cdr order-by))))))

#+l(defun range-to-offset (range)
  "Converts the 'range' argument to SQL OFFSET."
  (when range
    (car range)))

#+l(defun range-to-limit (range)
  "Converts the 'range' argument to SQL LIMIT."
  (when range
    (- (cdr range) (car range))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Querying persistent objects ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#+l(defmethod find-persistent-object-by-id ((store database) class-name object-id)
  (car (select class-name
	       :where [= (sql-expression :attribute (class-id-slot-name class-name))
	                 object-id]
	       :flatp t :caching nil :database store))
  )


(defun get-object-by-uuid-from-wilbur-mediator (class uuid)
  (let ((obj (make-instance class :repository (rdf:wilbur-mediator)))
        (graph))

    (setf graph (loop for i in (wilbur:db-triples (rdf:mediator-repository (rdf:wilbur-mediator))) 
                      if (equal (rdf:subject i) (rdf:repository-value (rdf:wilbur-mediator) uuid))
                      collect i))

    (when graph 
      (setf (slot-value obj 'de.setf.resource.implementation::uri)
            (uuid:make-uuid-from-string (ppcre:regex-replace-all "^urn:uuid:" (wilbur:node-uri (rdf:subject (first graph))) ""))))

    (rdf:project-graph graph obj)
    obj))

(defun get-all-urns-from-wilbur-mediator ()
  (remove-duplicates 
    (loop for i in (wilbur:db-triples (rdf:mediator-repository (rdf:wilbur-mediator))) 
          if (ppcre:scan "urn:uuid" (wilbur:node-uri (rdf:subject i)))
          collect (rdf:subject i))
    :test #'rdf:equal))

(defun get-all-rdf-items-from-wilbur-mediator (class-name)
  (loop for i in (get-all-urns-from-wilbur-mediator)
        collect (get-object-by-uuid-from-wilbur-mediator class-name i)))

(defmethod find-persistent-objects ((store rdf:repository-mediator) 
                                    class-name &key order-by range where &allow-other-keys)
  (let ((items (get-all-rdf-items-from-wilbur-mediator class-name)))
    (if order-by 
      (setf items (sort items 
                        (lambda (item1 item2)
                          (let ((function (if (equal (cdr order-by) 'desc) #'string> #'string<))
                                (value1 (slot-value item1 (car order-by)))
                                (value2 (slot-value item2 (car order-by))))

                            (unless (stringp value1)
                              (setf value1 (write-to-string value1)))

                            (unless (stringp value2)
                              (setf value2 (write-to-string value2)))

                            (funcall function value1 value2))))))

    (if range 
      (setf items (subseq items (car range) (cdr range))))

    (if (or where)
      (error "Unimplemented case ~A ~A ~A~%" order-by range where)
      items)))

(defmethod count-persistent-objects 
  ((store rdf:repository-mediator) 
   class-name
   &key where
   &allow-other-keys)
  (if where 
    (error "Unimplemented case with where ~A" where)
    (length (get-all-rdf-items-from-wilbur-mediator class-name))))

(defmethod object-id ((obj rdf:resource-object))
  (write-to-string (rdf:uri obj)))

(defmethod class-visible-slots-impl :around (obj &key readablep writablep)
  (if (and 
        (subtypep obj 'rdf:resource-object)
        (not (equal obj (find-class 'rdf:resource-object))))
    (let* ((slots (call-next-method))
           (tmp-slot)
           (allowed-slots-strings 
             (loop for i in slots 
                   if (ppcre:register-groups-bind 
                        (first)
                        ("([^\\.]+)\\.STATEMENT" (string (weblocks::slot-definition-name i)))
                        (setf tmp-slot first))
                   collect tmp-slot)))

      (loop for i in slots 
            if (find (string (weblocks::slot-definition-name i)) allowed-slots-strings :test #'string=)
            collect i))
    (call-next-method)))
