(eval-when (:compile-toplevel :load-toplevel)
  (sys.int::cal "home/med/package.lisp"))

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
      (compile-file path))
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

(cal "home/med/line.lisp")
(cal "home/med/mark.lisp")
(cal "home/med/editor.lisp")
(cal "home/med/save-excursion.lisp")
(cal "home/med/buffer.lisp")
(cal "home/med/buffer-stream.lisp")
(cal "home/med/point.lisp")
(cal "home/med/minibuffer.lisp")
(cal "home/med/redisplay.lisp")
(cal "home/med/commands.lisp")
(cal "home/med/keybindings.lisp")
(cal "home/med/repl.lisp")
(cal "home/med/grep.lisp")
(cal "home/med/find-definition.lisp")
(cal "home/med/main.lisp")

(defun make ()
   (cal "home/med/all.lisp"))
