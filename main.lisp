(in-package :med)

(defun translate-command (character)
  "Translate a character to a command."
  (let ((command (gethash character (buffer-key-map (current-buffer *editor*)))))
    (unless command
      (setf command (gethash character (global-key-map))))
    command))

(defun editor-loop ()
  (loop
     (let* ((*this-character* (editor-read-char))
            (*this-chord* (list *this-character*))
            (*this-command* (translate-command *this-character*)))
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
                            :damage-function (mezzano.gui.widgets:default-damage-function 
                                                window)))
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
         (initialize-key-map (global-key-map))
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
                                              (- (mezzano.gui.compositor:width window) 
                                                 left right)
                                              (- (mezzano.gui.compositor:height window) 
                                                 top bottom)))
    (switch-to-buffer (get-buffer-create "*scratch*"))
    (let ((buffer (get-buffer-create "*Messages*")))
      (insert buffer "Welcome to the Mezzano EDitor. Happy Hacking!")
      (insert buffer #\Newline))
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
  (mezzano.supervisor:make-thread
    (lambda () (editor-main width height initial-file))
    :name "Editor"
    :initial-bindings `((*terminal-io* ,(make-instance 'buffer-stream 
                                                       :buffer-name "*Messages*"))
                        (*standard-input* ,(make-synonym-stream '*terminal-io*))
                        (*standard-output* ,(make-synonym-stream '*terminal-io*))
                        (*error-output* ,(make-synonym-stream '*terminal-io*))
                        (*trace-output* ,(make-synonym-stream '*terminal-io*))
                        (*debug-io* ,(make-synonym-stream '*terminal-io*))
                        (*query-io* ,(make-synonym-stream '*terminal-io*)))))
