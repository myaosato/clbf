(uiop/package:define-package :clbf/clbf (:nicknames) (:use :cl) (:shadow)
                             (:export :bf-with-in :bf-cli) (:intern))
(in-package :clbf/clbf)
;;don't edit above

(defvar *instruction-pointer* 0)
(defvar *array* (make-array '(30000) :element-type '(unsigned-byte 8) :initial-element 0))
(defvar *data-pointer* 0)

(defvar *bf-operators* (list #\> #\< #\+ #\- #\. #\, #\[ #\]))
(defvar *bf-program*)

(defun clear-vars ()
  (setf *instruction-pointer* 0)
  (setf *array* (make-array '(30000) :element-type '(unsigned-byte 8) :initial-element 0))
  (setf *data-pointer* 0)
  (setf *bf-program* nil))

(defun is-bf-op (char)
  (find char *bf-operators*))

(defun bf-read-program (&optional (stream *standard-input*))
  (do ((c (read-char stream nil nil) (read-char stream nil nil))
       (lst nil (if (is-bf-op c) (cons c lst) lst)))
      ((null c) (coerce (reverse lst) 'string))))

(defun bf-load-program (&optional (stream *standard-input*))
  (setf *bf-program* (bf-read-program stream)))

(defun bf-load-program-from-file (filespec)
  (with-open-file (in filespec)
    (bf-load-program in)))

(defun read-bf-program ()
  (ignore-errors (char *bf-program* *instruction-pointer*)))

(defun find-while ()
  (do ((cnt 0))
      ((< cnt 0))
    (decf *instruction-pointer*)
    (let ((c (char *bf-program* *instruction-pointer*)))
      (cond ((char= c #\[)
             (decf cnt))
            ((char= c #\])
             (incf cnt))))))

(defun find-end ()
  (do ((cnt 0))
      ((< cnt 0))
    (incf *instruction-pointer*)
    (let ((c (char *bf-program* *instruction-pointer*)))
      (cond ((char= c #\])
             (decf cnt))
            ((char= c #\[)
             (incf cnt))))))

(defun bf-op-> ()
  (incf *data-pointer*))

(defun bf-op-< () 
  (if (> *data-pointer* 0) (decf *data-pointer*) 0))

(defun bf-op-+ () 
  (incf (aref *array* *data-pointer*)))

(defun bf-op-- () 
  (decf (aref *array* *data-pointer*)))

(defun bf-op-out (&optional (stream *standard-output*)) 
  (write-char (code-char (aref *array* *data-pointer*)) stream))

(defun bf-op-in (&optional (stream *standard-input*)) 
  (setf (aref *array* *data-pointer*) (char-code (read-char stream))))

(defun bf-op-while ()
  (if (= (aref *array* *data-pointer*) 0)
      (find-end)))

(defun bf-op-end ()
  (if (/= (aref *array* *data-pointer*) 0)
      (find-while)))
   
(defun bf-core (&key (in-stream *standard-input*) (out-stream *standard-output*))
  (do ((op (read-bf-program) (read-bf-program)))
      ((null op) t)
    (cond ((char= op #\>) (bf-op->))
          ((char= op #\<) (bf-op-<))
          ((char= op #\+) (bf-op-+))
          ((char= op #\-) (bf-op--))
          ((char= op #\.) (bf-op-out out-stream))
          ((char= op #\,) (bf-op-in in-stream))
          ((char= op #\[) (bf-op-while))
          ((char= op #\]) (bf-op-end)))
    (incf *instruction-pointer*)))

(defun bf-cli (argv)
  (etypecase argv
    (list
     (bf-cli (first argv)))
    ((or string pathname)
     (bf-load-program-from-file argv)
     (bf-core)
     (clear-vars))))

(defun bf-with-in (stream &key (in-stream *standard-input*) (out-stream *standard-output*))
  (bf-load-program stream)
  (bf-core :in-stream in-stream :out-stream out-stream))

