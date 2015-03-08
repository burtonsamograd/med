(defun cal (file)
   (handler-bind
     ;; automatically choose 'smash existing class' when loading
     ((t (lambda (c) (invoke-restart (find-restart 'continue)))))
    (sys.int::cal file)))

(cal "home/me/package.lisp")
(cal "home/me/line.lisp")
(cal "home/me/mark.lisp")
(cal "home/me/editor.lisp")
(cal "home/me/buffer.lisp")
(cal "home/me/point.lisp")
(cal "home/me/minibuffer.lisp")
(cal "home/me/redisplay.lisp")
(cal "home/me/commands.lisp")
(cal "home/me/keybindings.lisp")
(cal "home/me/main.lisp")
