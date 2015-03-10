(in-package :med)

(defvar *repl-key-map*)

(defun start-repl ()
  (initialize-repl-key-map)repl.lisp

  (let ((buffer (get-buffer "*repl*")))
    (unless buffer
      (setf buffer (make-instance 'buffer
                                  :key-map *repl-key-map*))
      (setf (buffer-property buffer 'name) "*repl*")
      (push buffer (buffer-list)))
  (switch-to-buffer buffer)))

(defun repl-finish-input-command ())

(defun initialize-repl-key-map ()
  (setf *repl-key-map* (make-hash-table))
  (set-key #\C-M 'repl-finish-input-command *repl-key-map*)
)