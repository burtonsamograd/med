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

(defun read-from-minibuffer (prompt &optional default-text)
  "Read a string from the minibuffer."
  (let ((old-buffer (current-buffer *editor*)))
    (when (eql old-buffer *minibuffer*)
      (error "Recursive minibuffer read!"))
    (unwind-protect
         (progn
           (setf *minibuffer* 
                 (make-instance 'buffer
                                :key-map *minibuffer-key-map*
                                :post-command-hooks '(fix-minibuffer-point-position-hook)))
           (setf (buffer-property *minibuffer* 'name) "*Minibuffer*")
           (switch-to-buffer *minibuffer*)
           (insert *minibuffer* prompt)
           (setf (buffer-property *minibuffer* 'minibuffer-prompt-end) 
                                  (copy-mark (buffer-point *minibuffer*) :left))
           (when default-text
             (insert *minibuffer* default-text))
           (catch 'minibuffer-result
             (editor-loop)))
      (switch-to-buffer old-buffer))))


(defun minibuffer-yes-or-no-p (&optional control &rest arguments)
  (let ((prompt (apply 'format nil control arguments)))
    (loop
       (let ((line (read-from-minibuffer (format nil "~A (Yes or No) " prompt))))
         (cond ((string-equal line "yes")
                (return t))
               ((string-equal line "no")
                (return nil)))))))

(defun minibuffer-y-or-n-p (&optional control &rest arguments)
  (let ((prompt (apply 'format nil control arguments)))
    (loop
       (let ((line (read-from-minibuffer (format nil "~A (Y or N) " prompt))))
         (cond ((string-equal line "y")
                (return t))
               ((string-equal line "n")
                (return nil)))))))

(defun initialize-minibuffer-key-map (key-map)
  (set-key #\Newline 'minibuffer-finish-input-command key-map)
  (set-key #\C-M 'minibuffer-finish-input-command key-map)
  (set-key '(#\C-X #\C-F) nil key-map)
  (set-key '(#\C-X #\C-S) nil key-map)
  (set-key '(#\C-X #\C-W) nil key-map)
  (set-key '(#\C-X #\k) nil key-map)
  (set-key '(#\C-X #\b) nil key-map)
  (set-key '(#\C-X #\C-B) nil key-map)
  (set-key #\C-C nil key-map))
