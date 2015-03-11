(in-package :med)

(defvar *mark-stack* ())

(defun function-source-file (function-symbol)
   (read-from-string 
     (sixth (sys.int::function-pool-object (symbol-function function-symbol) 1))))

(defun function-top-level-form-number (function-symbol)
   (seventh (sys.int::function-pool-object (symbol-function function-symbol) 1)))

(defun find-definition (function-symbol)
  (let ((file (function-source-file function-symbol))
        (form (function-top-level-form-number function-symbol)))
    (find-file file)
    (let ((buffer (current-buffer *editor*)))
      (move-beginning-of-buffer buffer)
      (dotimes (i (1+ form))
        (move-sexp buffer))
    (move-sexp buffer -1))))

(defun find-definition-command ()
  (find-definition (read-from-string (symbol-at-point (current-buffer *editor*)))))



