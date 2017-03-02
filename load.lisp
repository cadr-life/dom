;;; -*- mode:lisp;  coding:utf-8 -*-

(export '(load xhr-receive))


(defun xhr-receive (uri fn-ok &optional fn-err)
    (let* ((req (#j:opNew #j:window "XMLHttpRequest")))
        (funcall ((oget req "open" "bind") req "GET" uri t))
        (funcall ((oget req "setRequestHeader" "bind") req "Cache-Control" "no-cache"))
        (funcall ((oget req "setRequestHeader" "bind") req "Cache-Control" "no-store"))
        (setf (oget req "onreadystatechange")
              (lambda (evt)
                  (if (= (oget req "readyState") 4)
                      (if (= (oget req "status") 200)
                          (progn
                              (funcall fn-ok (oget req "responseText")))
                          (if (not (null fn-err))
                              (funcall fn-err uri (oget req "status") )
                              (#j:console:log (concat "\n" (oget req "status") ": Error load " name " - " (oget req "statusText"))))))))
        (#j:reqXHRsendNull req) ))



(defun eval-form (input)
    (%js-try
     (handler-case
         (progn
             (let* ((forms (read-from-string (concat "(" input ")")))
                    (result nil))
                 ;;(format t "length forms ~a ~%" (length forms))
                 (dolist (x forms)
                     ;;(format t "Level0: form: ~s~%" x)
                     (setf result (multiple-value-list (eval-interactive x)))
                     ;;(format t "result eval ~s~%" result)
                     (dolist (y result)
                         (format t "~a ~%" y)))))
       (warning (msg)
           (format t "Warning: ~s~%" (!condition-args msg)))
       (error (msg)
           (format t "Error: ~s~%" (!condition-args msg))))
     (catch (err)
         (format t "Catch js error: ~s~%" (or (oget err "message") err)))
     (finally
      (format t "Finalyze~%")) ))



(defun load (host-file-name)
    (xhr-receive  host-file-name
                  (lambda (input)
                      (eval-form (substitute #\Space (code-char 13) input) ))
                  (lambda (uri status)
                      (format t "~%Load: Can't load ~s. Status: ~a~%" uri status)) )
    (values))
