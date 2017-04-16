(in-package :weblocks)

(export '(url-for named-url-for *routes-mapper* main-route connect connect-named))

(defclass priority-generation-mapper (routes:mapper)
  ((routes :initform nil)
   (routes-by-name :initform (make-hash-table))))

(defparameter *routes-mapper* (make-instance 'priority-generation-mapper))

(defmethod get-min-priority ((self priority-generation-mapper))
  (with-slots (routes) self
    (or 
      (loop for i in routes maximize (cdr i))
      0)))

(defmethod connect ((mapper priority-generation-mapper) route)
  (push (cons route (1- (get-min-priority mapper))) 
        (slot-value mapper 'routes))

  (routes:connect mapper route))

(defmethod connect-named (name (mapper priority-generation-mapper) route)
  (setf (gethash name (slot-value mapper 'routes-by-name)) route)

  (connect mapper route))

(defun get-vars-from-route-template (template)
  (loop for i in template
        append (cond 
                 ((routes::variable-p i) (list (routes:template-data i)))
                 ((typep i 'routes:concat-template)
                  (get-vars-from-route-template (routes:template-data i)))
                 (t nil))))

(defmethod route-matches-variables-p (route vars)
  (let ((vars-keys (mapcar #'car vars))
        (variables (get-vars-from-route-template (routes:route-template route))))
    (every (lambda (item)
             (find item variables)) vars-keys)))

(defclass main-route (routes:route)
  ())

(defun get-routes-with-variables (vars)
  (loop for (route . priority) in (reverse (slot-value *routes-mapper* 'routes))
        if (route-matches-variables-p route vars)
        collect route))

(defun get-route-by-name (route-name)
  (gethash route-name (slot-value *routes-mapper* 'routes-by-name)))

(defun url-for (&rest args)
  (flet ((transform-cdrs-to-string (alist)
           (loop for (key . val) in alist 
                 collect (cons key (if (stringp val) 
                                     val 
                                     (write-to-string val))))))
    (let* ((args-alist (transform-cdrs-to-string (alexandria:plist-alist args)))
           (route (first (get-routes-with-variables args-alist))))
      (unless route 
        (error "No route found for ~A" args))
      (format nil 
        "/~A"
        (routes::generate-url 
          route
          args-alist)))))

(defun named-url-for (url-name &rest args)
  (flet ((transform-cdrs-to-string (alist)
           (loop for (key . val) in alist 
                 collect (cons key (if (stringp val) 
                                       val 
                                       (write-to-string val))))))
    (let* ((args-alist (transform-cdrs-to-string (alexandria:plist-alist args)))
           (route (get-route-by-name url-name)))
      (unless route 
        (error "No route found for ~A" args))
      (format nil 
        "/~A"
        (routes::generate-url 
          route
          args-alist)))))
