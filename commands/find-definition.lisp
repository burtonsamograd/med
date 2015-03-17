(in-package :med)

(defvar *mark-stack* ())

(defun function-source-file (function-symbol)
  (let ((string (sixth (sys.int::function-pool-object
                         (symbol-function function-symbol) 1))))
    (when string
      (if (eql (char string 0) #\#)
        (read-from-string string) ; convert pathname
        (pathname string)))))

(defun function-top-level-form-number (function-symbol)
   (seventh (sys.int::function-pool-object (symbol-function function-symbol) 1)))

(defun find-definition (function-symbol)
  (let* ((buffer (current-buffer *editor*)))
    (let ((file (function-source-file function-symbol))
          (form (function-top-level-form-number function-symbol)))
      (if (and file form) 
        (progn
          (format t "~A ~A ~A ~A~%" buffer *package* file form)
          (let ((buffer (find-file file)))
            (move-beginning-of-buffer buffer)
            (move-sexp buffer (1+ form))
            (move-sexp buffer -1))
        (format t "Cannot find definition for function ~A" function-symbol))))))

(defun find-definition-command ()
  (find-definition (read-from-string (symbol-at-point (current-buffer *editor*)))))
