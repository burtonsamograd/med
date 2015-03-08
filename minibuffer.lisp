(in-package :med)

;;; Minibuffer stuff.

(defun fix-minibuffer-point-position-hook ()
  (when (mark< (buffer-point *minibuffer*)
               (buffer-property *minibuffer* 'minibuffer-prompt-end))
    (point-to-mark *minibuffer*
                   (buffer-property *minibuffer* 'minibuffer-prompt-end)))
  (when (mark< (buffer-mark *minibuffer*)
               (buffer-property *minibuffer* 'minibuffer-prompt-end))
    (move-mark-to-mark (buffer-mark *minibuffer*)
                       (buffer-property *minibuffer* 'minibuffer-prompt-end))))

(defun minibuffer-finish-input-command ()
  (move-end-of-buffer *minibuffer*)
  (throw 'minibuffer-result
    (buffer-string *minibuffer*
                   (buffer-property *minibuffer* 'minibuffer-prompt-end)
                   (buffer-point *minibuffer*))))

(defun read-from-minibuffer (prompt)
  "Read a string from the minibuffer."
  (let ((old-key-map (global-key-map *editor*))
        (old-buffer (current-buffer *editor*))
        (old-post-command-hooks (post-command-hooks *editor*)))
    (when (eql old-buffer *minibuffer*)
      (error "Recursive minibuffer read!"))
    (unwind-protect
         (progn
           (setf *minibuffer* (make-instance 'buffer))
           (setf (global-key-map *editor*) *minibuffer-key-map*
                 (buffer-property *minibuffer* 'name) "*Minibuffer*")
           (push 'fix-minibuffer-point-position-hook (post-command-hooks *editor*))
           (switch-to-buffer *minibuffer*)
           (insert *minibuffer* prompt)
           (setf (buffer-property *minibuffer* 'minibuffer-prompt-end) (copy-mark (buffer-point *minibuffer*) :left))
           (catch 'minibuffer-result
             (editor-loop)))
      (switch-to-buffer old-buffer)
      (setf (global-key-map *editor*) old-key-map
            (post-command-hooks *editor*) old-post-command-hooks))))

(defun minibuffer-yes-or-no-p (&optional control &rest arguments)
  (let ((prompt (apply 'format nil control arguments)))
    (loop
       (let ((line (read-from-minibuffer (format nil "~A (Yes or No) " prompt))))
         (cond ((string-equal line "yes")
                (return t))
               ((string-equal line "no")
                (return nil)))))))

