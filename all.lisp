(sys.int::cal "home/med/package.lisp")

(in-package :med)

(defmacro awhen (cond &body body)
  `(let ((it ,cond))
     (when it
       ,@body)))

(defun cal (file)
   (handler-bind
     ;; automatically choose 'smash existing class' when loading
     ((t (lambda (c) 
           (declare (ignore c))
           (awhen (find-restart 'continue)
             (invoke-restart it)))))
    (sys.int::cal file)))

(cal "home/med/line.lisp")
(cal "home/med/mark.lisp")
(cal "home/med/editor.lisp")
(cal "home/med/buffer.lisp")
(cal "home/med/buffer-stream.lisp")
(cal "home/med/point.lisp")
(cal "home/med/minibuffer.lisp")
(cal "home/med/redisplay.lisp")
(cal "home/med/commands.lisp")
(cal "home/med/keybindings.lisp")
(cal "home/med/main.lisp")

(defun make ()
   (cal "home/med/all.lisp"))