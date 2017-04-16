Using routes branch and Weblocks CMS is possible only when using this code 

```lisp
(defmethod (setf weblocks-twitter-bootstrap-application::bootstrap-navbar-selector-selected) 
  :around (value (obj weblocks-twitter-bootstrap-application:bootstrap-navbar-selector))
  "This is used to make navbar selector work without routes"
  )


(connect *routes-mapper* (make-instance 'weblocks::main-route :template (routes:parse-template "")))
(connect *routes-mapper* (routes:make-route "/admin/:(toplevel.selected)"))
;(connect-named :user-collections *routes-mapper* (routes:make-route "/:(user.current)/"))

; Example of init-session for admin
(defun/cc init-admin-user-session (root)
  (when (weblocks-cms:weblocks-cms-access-granted)
    (do-page 
      (let ((selector (apply 
                        #'weblocks-twitter-bootstrap-application:make-navbar-selector 
                        (append 
                          (list "toplevel")
                          (weblocks-cms:weblocks-cms-admin-menu)))))
        (setf (weblocks::widget-uri-id selector) :admin-selector)
        selector))))
```


