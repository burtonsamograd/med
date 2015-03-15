(eval-when (:compile-toplevel :load-toplevel)
  (sys.int::cal "package.lisp"))

(in-package :med)

(defmacro awhen (cond &body body)
  `(let ((it ,cond))
     (when it
       ,@body)))

(defun cal-1 (path)
  "Compile and load PATH.
If the compiled file is out of date, recompile and load it."
  (let ((compiled (compile-file-pathname path)))
    (when (or (not (probe-file compiled))
              (<= (file-write-date compiled) (file-write-date path)))
      (format t "; Compiling ~S~%" path)
      (ignore-errors (delete-file compiled))
      (let ((*standard-output* (make-broadcast-stream)))
        (compile-file path)))
    (format t "; Loading ~S~%" compiled)
    (load compiled)))

(defun cal (file)
   (handler-bind
     ;; automatically choose 'smash existing class' when loading
     ((t (lambda (c)
           (declare (ignore c))
           (awhen (find-restart 'continue)
             (invoke-restart it)))))
     (cal-1 file)))

(defun make ()
  (let ((start-time (get-universal-time)))
     (cal "all.lisp")
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
(cal "keybindings.lisp")
(cal "repl.lisp")
(cal "grep.lisp")
(cal "find-definition.lisp")
(cal "main.lisp")


