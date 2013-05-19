
(defpackage #:weblocks-d-s-r
  (:use :cl  :weblocks)
  (:documentation
   "A driver for weblocks backend store API that connects to de.setf.resource."))

(in-package :weblocks-d-s-r)

(defvar *items* nil)
(defvar *max-items-length* 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Initialization/finalization ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod open-store ((store-type (eql :d-s-r)) &key (mediator-type (eql :wilbur-n3-file)) pathname)
  (with-open-file (in pathname :direction :input)
    (rdf:load-repository-as (rdf:mediator-repository (rdf:wilbur-mediator)) in mime:application/n3))
  (setf *default-store* (rdf:wilbur-mediator)))

(defmethod close-store ((store rdf:repository-mediator))
  (when (eq *default-store* store)
    (setf *default-store* nil))
  (rdf:repository-close store))

#+l(defmethod clean-store ((store database)))

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
(defmethod persist-object ((store rdf:repository-mediator) object &key)
  (rdf:commit object)
  (setf (slot-value object 'de.setf.resource.implementation::state) rdf:new-persistent)
  (setf (slot-value object 'de.setf.resource.implementation::history) rdf:new-persistent)
  (rdf:project-graph store object)
  (setf *items* nil)
  object)

#+l(defmethod delete-persistent-object ((store database) object))

#+l(defmethod delete-persistent-object-by-id ((store database) class-name object-id))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Querying persistent objects ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod find-persistent-object-by-id ((store rdf:repository-mediator) class-name object-id)
  (get-object-by-uuid-from-wilbur-mediator class-name (wilbur:node (uuid::format-as-urn nil (uuid:make-uuid-from-string object-id)))))


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
    (rdf:make-persistent obj)
    obj))

(defun get-all-urns-from-wilbur-mediator ()
  (remove-duplicates 
    (loop for i in (wilbur:db-triples (rdf:mediator-repository (rdf:wilbur-mediator))) 
          if (ppcre:scan "urn:uuid" (wilbur:node-uri (rdf:subject i)))
          collect (rdf:subject i))
    :test #'rdf:equal))

(defun get-all-rdf-items-from-wilbur-mediator (class-name)
  (when (or 
          (not *items*)
          (< (length *items*) *max-items-length* )) 
    (setf *items* 
          (loop for i in (get-all-urns-from-wilbur-mediator)
                collect (get-object-by-uuid-from-wilbur-mediator class-name i)))
    (setf *max-items-length* (length *items*)))

  *items*)

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
