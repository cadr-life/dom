;;;
;;; sketch
;;;
;;; (man "man")
;;;
(defparameter *man-db
  (make-js-object "man" "man/man0.txt" "dom" "man/dom0.txt"))


(defun man-prefix (topic)
    (concat "man-" topic))


(defun load-man (man host-file-name)
    (xhr-receive  host-file-name
                  (lambda (input)
                      (let* ((txt)) 
                           (setf txt (split-str (list #\\)
                                            (substitute #\Space (code-char 13)
                                                        (substitute #\Space (code-char 10) input)) ))
                           (res-alloc man txt)
                           (display-man-topic txt)))
                  (lambda (uri status)
                      (format t "~%Load: Can't load ~s. Status: ~a~%" uri status)) )
    (values))


(defun display-man-topic (lst)
    (dolist (str lst)
        (format t "~a~%" str)))

(defun check-topic (name)
   (let* ((fname (oget *man-db name)))
     (if fname fname "man/man0.txt")))


(defun man (topic)
    (let* ((res-pref (man-prefix topic))
           (hm (res-refer res-pref)))
        (cond  ((not (null hm)) (display-man-topic hm))
               (t (load-man res-pref (check-topic topic))))
        (values)))
