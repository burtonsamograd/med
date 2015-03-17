(eval-when (:compile-toplevel :load-toplevel)
  (unless (find-package :med)
    (sys.int::cal "package.lisp")))

(in-package :med)

(defmacro awhen (cond &body body)
  `(let ((it ,cond))
     (when it
       ,@body)))

(defvar *loaded-files* ())

(defun cal-1 (path force-load)
  "Compile and load PATH.
   If the compiled file is out of date, recompile and load it."
  (let ((compiled-file (compile-file-pathname path))
        file-was-compiled)
    (when (or (not (probe-file compiled-file))
              (<= (file-write-date compiled-file) (file-write-date path)))
      (format t "; Compiling ~S~%" path)
      (ignore-errors (delete-file compiled-file))
      (let ((*standard-output* (make-broadcast-stream)))
        (compile-file path))
      (setf file-was-compiled t))
    (when (or file-was-compiled
              force-load
              (not (member compiled-file *loaded-files* :test #'equal)))
      (format t "; Loading ~S~%" compiled-file)
      (pushnew compiled-file *loaded-files* :test #'equal)
      (load compiled-file))))

(defun cal (file &optional force-load)
   (handler-bind
     ;; automatically choose 'smash existing class' when loading
     ((t (lambda (c)
           (declare (ignore c))
           (awhen (find-restart 'continue)
             (invoke-restart it)))))
     (cal-1 file force-load)))

(defun make (&optional force-load)
  (when force-load 
    (setf *loaded-files* nil))
  (let ((start-time (get-universal-time)))
     (cal "all.lisp" t)
     (format t "Total build time: ~A seconds.~%" (- (get-universal-time) start-time))))

(defun clean ()
  (dolist (f (directory "*.llf")) (delete-file f)))

(cal "line.lisp")
(cal "mark.lisp")
(cal "editor.lisp")
(cal "save-excursion.lisp")
(cal "buffer.lisp")
(cal "buffer-stream.lisp")
(cal "point.lisp")
(cal "minibuffer.lisp")
(cal "redisplay.lisp")
(cal "commands.lisp")
(cal "file.lisp")
(cal "keybindings.lisp")
(cal "repl.lisp")
(cal "grep.lisp")
(cal "find-definition.lisp")
(cal "isearch.lisp")
(cal "main.lisp")


