(uiop/package:define-package :clbf/2bf (:use :cl))
(in-package :clbf/2bf)
;;;don't edit above

(defun 2bf-dup () 
  (format nil "<[>+>+<<-]>>[<<+>>-]"))

(defun 2bf-push (num)
  (if (or (< num 0) (< 255 num)) (return-from 2bf-push nil))
  (format nil "~{~A~}>" (make-list num :initial-element "+")))

(defun 2bf-pop ()
  (format nil ".<"))



