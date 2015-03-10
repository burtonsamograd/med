(in-package :med)

(defun start-repl ()
  (let ((buffer (get-buffer "*repl*")))
    (unless buffer
      (make-instance 'buffer :name "*repl*"))
  (switch-to-buffer buffer)))