(in-package :me)

(defun translate-command (editor character)
  "Translate a character to a command."
  (gethash character (global-key-map editor)))

(defun editor-loop ()
  (loop
     (let* ((*this-character* (editor-read-char))
            (*this-chord* (list *this-character*))
            (*this-command* (translate-command *editor* *this-character*)))
       (cond ((hash-table-p *this-command*)
              (loop
                 (setf *this-character* (editor-read-char)
                       *this-command* (gethash *this-character* *this-command*))
                 (push *this-character* *this-chord*)
                 (when (not (hash-table-p *this-command*))
                   (setf *this-chord* (reverse *this-chord*))
                   (cond (*this-command*
                          (mapc 'funcall (pre-command-hooks *editor*))
                          (funcall *this-command*)
                          (mapc 'funcall (post-command-hooks *editor*)))
                         (t (format t "Unknown command ~S~%" *this-chord*)))
                   (return))))
             (*this-command*
              (mapc 'funcall (pre-command-hooks *editor*))
              (funcall *this-command*)
              (mapc 'funcall (post-command-hooks *editor*)))
             (t (format t "Unknown command ~S~%" *this-character*)))
       (setf *last-command* *this-command*
             *last-character* *this-character*
             *last-chord* *this-chord*)
       (setf (pending-redisplay *editor*) (not (redisplay))))))

(defun set-key (key fn map)
  (cond ((not (consp key))
         (set-key (list key) fn map))
        ((rest key)
         (let ((next (gethash (first key) map)))
           (when (not (hash-table-p next))
             (setf next (make-hash-table)
                   (gethash (first key) map) next))
           (set-key (rest key) fn next)))
        (t (setf (gethash (first key) map) fn))))

(defun initialize-key-map (key-map)
  (set-key #\Newline 'self-insert-command key-map)
  ;; ASCII printable characters.
  (loop for i from #x20 to #x7E
     do (set-key (code-char i) 'self-insert-command key-map))
  ;; Latin 1 printable characters.
  (loop for i from #xA0 to #xFF
     do (set-key (code-char i) 'self-insert-command key-map))
  (set-key #\C-F 'forward-char-command key-map)
  (set-key #\Right-Arrow 'forward-char-command key-map)
  (set-key #\C-B 'backward-char-command key-map)
  (set-key #\Left-Arrow 'backward-char-command key-map)
  (set-key #\C-N 'next-line-command key-map)
  (set-key #\Down-Arrow 'next-line-command key-map)
  (set-key #\C-P 'previous-line-command key-map)
  (set-key #\Up-Arrow 'previous-line-command key-map)
  (set-key #\M-F 'forward-word-command key-map)
  (set-key #\M-B 'backward-word-command key-map)
  (set-key #\C-M-F 'forward-sexp-command key-map)
  (set-key #\C-M-B 'backward-sexp-command key-map)
  (set-key #\C-A 'move-beginning-of-line-command key-map)
  (set-key #\C-E 'move-end-of-line-command key-map)
  (set-key #\C-K 'kill-line-command key-map)
  (set-key #\C-M-K 'kill-sexp-command key-map)
  (set-key #\C-Q 'quoted-insert-command key-map)
  (set-key #\C-L 'recenter-command key-map)
  (set-key #\M-L 'redraw-screen-command key-map)
  (set-key #\C-Space 'set-mark-command key-map)
  (set-key '(#\C-X #\C-X) 'exchange-point-and-mark-command key-map)
  (set-key #\Backspace 'delete-backward-char-command key-map)
  (set-key #\C-D 'delete-forward-char-command key-map)
  (set-key #\Delete 'delete-forward-char-command key-map)
  (set-key #\C-Backspace 'backward-kill-word-command key-map)
  (set-key #\M-D 'forward-kill-word-command key-map)
  (set-key #\C-W 'kill-region-command key-map)
  (set-key #\C-Y 'yank-command key-map)
  (set-key '(#\C-X #\C-F) 'find-file-command key-map)
  (set-key '(#\C-X #\C-S) 'save-buffer-command key-map)
  (set-key '(#\C-X #\C-W) 'write-file-command key-map)
  (set-key '(#\C-X #\k) 'kill-buffer-command key-map)
  (set-key '(#\C-X #\b) 'switch-to-buffer-command key-map)
  (set-key '(#\C-X #\C-B) 'list-buffers-command key-map)
  (set-key #\C-G 'keyboard-quit-command key-map)
  (set-key #\M-< 'move-beginning-of-buffer-command key-map)
  (set-key #\Home 'move-beginning-of-buffer-command key-map)
  (set-key #\M-> 'move-end-of-buffer-command key-map)
  (set-key #\End 'move-end-of-buffer-command key-map)
  (set-key #\C-V 'scroll-up-command key-map)
  (set-key #\Page-Down 'scroll-up-command key-map)
  (set-key #\M-V 'scroll-down-command key-map)
  (set-key #\Page-Up 'scroll-down-command key-map)
  (set-key '(#\C-C #\C-C) 'eval-top-level-form-command key-map)
  (set-key '(#\C-C #\C-A) 'beginning-of-top-level-form-command key-map)
  (set-key #\C-S 'isearch-command key-map)
  (set-key #\M-W 'copy-region-command key-map)
  (set-key #\C-M 'newline-command key-map)
  (set-key #\C-J 'newline-command key-map)
  (set-key #\C-O 'open-line-command key-map)
  (set-key #\M-Backspace 'backward-kill-word-command key-map)
  (set-key #\M-Colon 'eval-expression-command key-map)
  (set-key '(#\C-C #\C-K) 'compile-buffer-command key-map)
  (set-key '(#\C-X #\C-E) 'eval-last-sexp-command key-map)
  (set-key #\M-O 'find-matching-paren-command key-map)
  (set-key #\M-FULL_STOP 'find-symbol-at-point-command key-map))

(defun initialize-minibuffer-key-map (key-map)
  (initialize-key-map key-map)
  (set-key #\Newline 'minibuffer-finish-input-command key-map)
  (set-key #\C-M 'minibuffer-finish-input-command key-map)
  (set-key '(#\C-X #\C-F) nil key-map)
  (set-key '(#\C-X #\C-S) nil key-map)
  (set-key '(#\C-X #\C-W) nil key-map)
  (set-key '(#\C-X #\k) nil key-map)
  (set-key '(#\C-X #\b) nil key-map)
  (set-key '(#\C-X #\C-B) nil key-map)
  (set-key #\C-C nil key-map))

(defun editor-main (width height initial-file)
  (mezzano.gui.font:with-font (font mezzano.gui.font:*default-monospace-font* mezzano.gui.font:*default-monospace-font-size*)
    (mezzano.gui.font:with-font (font-bold mezzano.gui.font::*default-monospace-bold-font* mezzano.gui.font:*default-monospace-font-size*)
      (let ((fifo (mezzano.supervisor:make-fifo 50)))
	(mezzano.gui.compositor:with-window (window fifo (or width 640) (or height 700) :kind :editor)
	  (let* ((framebuffer (mezzano.gui.compositor:window-buffer window))
		 (frame (make-instance 'mezzano.gui.widgets:frame
				       :framebuffer framebuffer
				       :title "Editor"
				       :close-button-p t
				       :damage-function (mezzano.gui.widgets:default-damage-function window)))
		 (*editor* (make-instance 'editor
					  :fifo fifo
					  :font font
					  :font-bold font-bold
					  :window window
					  :frame frame
					  :buffer (make-instance 'buffer)))
		 (*last-command* nil)
		 (*last-character* nil)
		 (*last-chord* nil)
		 (*minibuffer* (make-instance 'buffer))
		 (*minibuffer-key-map* (make-hash-table))
		 (*default-pathname-defaults* *default-pathname-defaults*))
	    (initialize-key-map (global-key-map *editor*))
	    (initialize-minibuffer-key-map *minibuffer-key-map*)
	    (mezzano.gui.widgets:draw-frame frame)
	    (multiple-value-bind (left right top bottom)
		(mezzano.gui.widgets:frame-size (frame *editor*))
	      (mezzano.gui:bitset (- (mezzano.gui.compositor:height window) top bottom)
				  (- (mezzano.gui.compositor:width window) left right)
				  (background-colour *editor*)
				  framebuffer
				  top left)
	      (mezzano.gui.compositor:damage-window window
						    left top
						    (- (mezzano.gui.compositor:width window) left right)
						    (- (mezzano.gui.compositor:height window) top bottom)))
	    (switch-to-buffer (get-buffer-create "*Scratch*"))
	    (ignore-errors
	      (when initial-file
		(find-file initial-file)))
	    (catch 'quit
	      (loop
		 (handler-case
		     (editor-loop)
		   (error (c)
		     (ignore-errors
		       (format t "Editor error: ~A~%" c)
		       (setf (pending-redisplay *editor*) t))))))))))))

(defun spawn (&key width height initial-file)
  (mezzano.supervisor:make-thread (lambda () (editor-main width height initial-file))
                                  :name "Editor"
                                  :initial-bindings `((*terminal-io* ,(make-instance 'mezzano.gui.popup-io-stream:popup-io-stream
                                                                                     :title "Editor console"))
                                                      (*standard-input* ,(make-synonym-stream '*terminal-io*))
                                                      (*standard-output* ,(make-synonym-stream '*terminal-io*))
                                                      (*error-output* ,(make-synonym-stream '*terminal-io*))
                                                      (*trace-output* ,(make-synonym-stream '*terminal-io*))
                                                      (*debug-io* ,(make-synonym-stream '*terminal-io*))
                                                      (*query-io* ,(make-synonym-stream '*terminal-io*)))))
