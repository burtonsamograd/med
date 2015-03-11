(in-package :med)

;;;; Begin command wrappers.

;;; Motion & mark commands.

(defun forward-char-command ()
  (move-char (current-buffer *editor*)))

(defun backward-char-command ()
  (move-char (current-buffer *editor*) -1))

(defun next-line-command ()
  (move-line (current-buffer *editor*)))

(defun previous-line-command ()
  (move-line (current-buffer *editor*) -1))

(defun forward-word-command ()
  (move-word (current-buffer *editor*)))

(defun backward-word-command ()
  (move-word (current-buffer *editor*) -1))

(defun forward-sexp-command ()
  (move-sexp (current-buffer *editor*)))

(defun backward-sexp-command ()
  (move-sexp (current-buffer *editor*) -1))

(defun move-beginning-of-line-command ()
  (move-beginning-of-line (current-buffer *editor*)))

(defun move-end-of-line-command ()
  (move-end-of-line (current-buffer *editor*)))

(defun move-beginning-of-buffer-command ()
  (move-beginning-of-buffer (current-buffer *editor*)))

(defun move-end-of-buffer-command ()
  (move-end-of-buffer (current-buffer *editor*)))

(defun set-mark-command ()
  (set-mark (current-buffer *editor*)))

(defun exchange-point-and-mark-command ()
  (exchange-point-and-mark (current-buffer *editor*)))

;;; Editing commands.

(defun self-insert-command ()
  (insert (current-buffer *editor*) *this-character*))

(defun quoted-insert-command ()
  (insert (current-buffer *editor*) (editor-read-char)))

(defun delete-forward-char-command ()
  (delete-char (current-buffer *editor*)))

(defun delete-backward-char-command ()
  (delete-char (current-buffer *editor*) -1))

(defun kill-line-command ()
  (kill-line (current-buffer *editor*)))

(defun kill-region-command ()
  (let ((buffer (current-buffer *editor*)))
    (kill-region buffer (buffer-point buffer) (buffer-mark buffer))))

(defun kill-sexp-command ()
  (let* ((buffer (current-buffer *editor*))
         (point (buffer-point buffer)))
    (with-mark (current point)
      (move-sexp buffer 1)
      (kill-region buffer current point))))

(defun forward-kill-word-command ()
  (let* ((buffer (current-buffer *editor*))
         (point (buffer-point buffer)))
    (with-mark (current point)
      (move-word buffer 1)
      (kill-region buffer current point))))

(defun backward-kill-word-command ()
  (let* ((buffer (current-buffer *editor*))
         (point (buffer-point buffer)))
    (with-mark (current point)
      (move-word buffer -1)
      (kill-region buffer current point))))

(defun yank-command ()
  (yank-region (current-buffer *editor*)))

;;; Display commands.

(defun recenter-command ()
  (recenter (current-buffer *editor*)))

(defun redraw-screen-command ()
  (redraw-screen))

(defun scroll-up-command ()
  ;; Find the display line at the bottom of the screen and recenter on that.
  (let ((current-screen (editor-current-screen *editor*))
        (point (buffer-point (current-buffer *editor*))))
    (dotimes (i (length current-screen))
      (let ((line (aref current-screen (- (length current-screen) i 1))))
        (when line
          (setf (mark-line point) (display-line-line line)
                (mark-charpos point) (display-line-start line))
          (recenter (current-buffer *editor*))
          (return))))))

(defun scroll-down-command ()
  ;; Recenter on the topmost display line.
  (let* ((current-screen (editor-current-screen *editor*))
         (line (aref current-screen 0))
         (point (buffer-point (current-buffer *editor*))))
    (setf (mark-line point) (display-line-line line)
          (mark-charpos point) (display-line-start line))
    (recenter (current-buffer *editor*))))

;;; Other commands.

(defun keyboard-quit-command ()
  (error "Keyboard quit."))

(defun find-file (path)
  (setf path (merge-pathnames path))
  (dolist (buffer (buffer-list))
    (when (equal (buffer-property buffer 'path) path)
      (switch-to-buffer buffer)
      (setf (buffer-property buffer 'default-pathname-defaults)
            (make-pathname :name nil :type nil :version :newest :defaults path))
      (return-from find-file)))
  (let ((buffer (make-instance 'buffer)))
    (if (pathname-name path)
      ;; read file
      (with-open-file (s path :if-does-not-exist nil)
        (cond (s
               (loop
                  (multiple-value-bind (line missing-newline-p)
                      (read-line s nil)
                    (when (not line)
                      (return))
                    (insert buffer line)
                    (when (not missing-newline-p)
                      (insert buffer #\Newline)))))
              (t (setf (buffer-property buffer 'new-file) t)))
         (rename-buffer buffer (file-namestring path)))
      ;; read directory
      (progn
        (insert buffer (format nil "Directory: ~A~%~%" path))
        (mapc (lambda (file)
                (let* ((file-name (file-namestring file))
                       (name (if file-name file-name (directory-namestring file))))
                  (insert buffer name)
                  (insert buffer #\Newline)))
          (directory (merge-pathnames "*.*" path)))
        (setf (buffer-property buffer 'new-file) t)
        (rename-buffer buffer (directory-namestring path))))
    (push buffer (buffer-list))
    (setf (buffer-property buffer 'path) path)
    (move-beginning-of-buffer buffer)
    ;; Loading the file will set the modified flag.
    (setf (buffer-modified buffer) nil)
    (switch-to-buffer buffer)
    (setf (buffer-property buffer 'default-pathname-defaults)
          (make-pathname :name nil :type nil :version :newest :defaults path))))

(defun find-file-command ()
  (find-file (read-from-minibuffer "Find file: " 
                                   (namestring 
                                     (or (buffer-property (current-buffer *editor*)                                                                  'default-pathname-defaults)
                                         *default-pathname-defaults*)))))

;; TODO: factor out the buffer saving from the below 3 functions into defun save-buffer

(defun save-buffer-command ()
  (let ((buffer (current-buffer *editor*)))
    (when (not (buffer-property buffer 'path))
      (let* ((path (read-from-minibuffer (format nil "Write file (default ~S): " 
                                                 (buffer-property buffer 'default-pathname-defaults))))
             (filespec (merge-pathnames path)))
        (rename-buffer buffer (file-namestring filespec))
        (setf (buffer-property buffer 'path) filespec)))
    (with-open-file (s (buffer-property buffer 'path)
                       :direction :output
                       :if-exists :new-version
                       :if-does-not-exist :create)
      (do ((line (first-line buffer) (next-line line)))
          ((not line))
        (write-sequence (map 'string #'car (data line)) s)
        (when (next-line line)
          (terpri s))))
    (setf (buffer-property buffer 'new-file) nil
          (buffer-modified buffer) nil)
    (format t "Wrote ~S~%" (buffer-property buffer 'path))))

(defun save-some-buffers-command ()
  (dolist (buffer (buffer-list))
    (when (and (buffer-modified buffer)
               (minibuffer-y-or-n-p 
                 (format nil "Save buffer ~A?" (buffer-property buffer 'name)))
               (buffer-property buffer 'path))
      (with-open-file (s (buffer-property buffer 'path)
                         :direction :output
                         :if-exists :new-version
                         :if-does-not-exist :create)
        (do ((line (first-line buffer) (next-line line)))
            ((not line))
          (write-sequence (map 'string #'car (data line)) s)
          (when (next-line line)
            (terpri s))))
        (setf (buffer-property buffer 'new-file) nil
              (buffer-modified buffer) nil)
        (format t "Wrote ~S~%" (buffer-property buffer 'path)))))

(defun write-file-command ()
  (let* ((buffer (current-buffer *editor*))
         (*default-pathname-defaults* (or (buffer-property buffer 'path)
                                          (buffer-property buffer 'default-pathname-defaults)
                                          *default-pathname-defaults*))
         (path (read-from-minibuffer "Write file: " 
                                     (namestring *default-pathname-defaults*)))
         (filespec (merge-pathnames path)))
    (rename-buffer buffer (file-namestring filespec))
    (setf (buffer-property buffer 'path) filespec)
    (with-open-file (s (buffer-property buffer 'path)
                       :direction :output
                       :if-exists :new-version
                       :if-does-not-exist :create)
      (do ((line (first-line buffer) (next-line line)))
          ((not line))
        (write-sequence (map 'string #'car (data line)) s)
        (terpri s)))
    (setf (buffer-property buffer 'new-file) nil
          (buffer-modified buffer) nil)
    (format t "Wrote ~S~%" (buffer-property buffer 'path))))

(defun list-buffers-command ()
  (let ((buffer (get-buffer-create "*Buffers*")))
    (switch-to-buffer buffer)
    ;; Clear the whole buffer.
    (delete-region buffer
                   (make-mark (first-line buffer) 0)
                   (make-mark (last-line buffer) (line-length (last-line buffer))))
    (dolist (b (buffer-list))
      (insert buffer (buffer-property b 'name))
      (insert buffer #\Newline))
    (setf (buffer-modified buffer) nil)))

(defun switch-to-buffer-command ()
  (let* ((default-buffer (or (last-buffer *editor*)
                             (current-buffer *editor*)))
         (name (string-trim " " (read-from-minibuffer (format nil "Buffer (default ~A): " (buffer-property default-buffer 'name)))))
         (other-buffer (if (zerop (length name))
                           default-buffer
                           (get-buffer-create name))))
    (when (not (eql (current-buffer *editor*) other-buffer))
      (setf (last-buffer *editor*) (current-buffer *editor*))
      (switch-to-buffer other-buffer))))

(defun kill-buffer-command ()
  (let* ((name (read-from-minibuffer (format nil "Buffer (default ~A): " (buffer-property (current-buffer *editor*) 'name))))
         (buffer (if (zerop (length name))
                     (current-buffer *editor*)
                     (or (get-buffer name)
                         (error "No buffer named ~S" name)))))
    (when (buffer-modified buffer)
      (when (not (minibuffer-yes-or-no-p "Buffer ~S modified, kill anyway?" (buffer-property buffer 'name)))
        (return-from kill-buffer-command)))
    (kill-buffer buffer)))

(defun get-buffer-create (name)
  (setf name (string name))
  (or (get-buffer name)
      (let ((buffer (make-instance 'buffer)))
        (setf (buffer-property buffer 'name) name)
        (push buffer (buffer-list))
        buffer)))

(defun get-buffer (name)
  (dolist (b (buffer-list))
    (when (string-equal (buffer-property b 'name) name)
      (return b))))

(defun kill-buffer (buffer)
  (setf (buffer-list) (remove buffer (buffer-list)))
  (when (eql buffer (last-buffer *editor*))
    (setf (last-buffer *editor*) nil))
  (when (eql buffer (current-buffer *editor*))
    (switch-to-buffer
     (if (buffer-list)
         (first (buffer-list))
         (get-buffer-create "*Scratch*")))))

(defun unique-name (name &optional version)
  (let ((actual-name (if version
                         (format nil "~A <~D>" name version)
                         name)))
    (if (get-buffer actual-name)
        (unique-name name (if version
                              (1+ version)
                              1))
        actual-name)))

(defun rename-buffer (buffer new-name)
  (unless (string-equal (buffer-property buffer 'name) new-name)
    (setf (buffer-property buffer 'name) (unique-name new-name))
    (refresh-title)))

;;; Lisp commands.

(defun beginning-of-top-level-form (buffer)
  "Move to the start of a top-level form.
A top-level form is designated by an open parenthesis at the start of a line."
  (let ((point (buffer-point buffer)))
    (setf (mark-charpos point) 0)
    (loop
       (when (eql (character-right-of point) #\()
         (return))
       (when (not (previous-line (mark-line point)))
         (error "Can't find start of top-level form."))
       (setf (mark-line point) (previous-line (mark-line point))))))

(defmacro save-excursion ((buffer) &body body)
  "Save the point & mark in buffer, execute body, then restore the saved point
and mark."
  `(call-with-save-excursion ,buffer (lambda () ,@body)))

(defun call-with-save-excursion (buffer fn)
  (let ((previous-point (copy-mark (buffer-point buffer) :right))
        (previous-mark (copy-mark (buffer-mark buffer) :left))
        (previous-mark-active (buffer-mark-active buffer)))
    (unwind-protect
         (funcall fn)
      (move-mark-to-mark (buffer-point buffer) previous-point)
      (move-mark-to-mark (buffer-mark buffer) previous-mark)
      (setf (buffer-mark-active buffer) previous-mark-active))))

(defun symbol-at-point (buffer)
  (save-excursion (buffer)
    (move-sexp buffer 1)
    (let ((point (copy-mark (buffer-point buffer))))
      (move-sexp buffer -1)
      (buffer-string buffer point (buffer-point buffer)))))

(defun search-forward (buffer string)
  "From point, search forwards for string in buffer."
  (let ((point (copy-mark (buffer-point buffer))))
    ;; Search to the end of the buffer
    (save-excursion (buffer)
       (move-end-of-buffer buffer)
        (setf pos (search string (buffer-string buffer point
                                                (buffer-point buffer)))))
    (if pos
       ;; Found the string, go there
       (move-char buffer (+ pos (length string)))
       ;; Didn't find it, wrap around and search from the beginning
       (progn
         (save-excursion (buffer)
           (move-beginning-of-buffer buffer)
           (setf pos (search string (buffer-string buffer (buffer-point buffer) point))))
         (when pos
           (move-beginning-of-buffer buffer)
           (move-char buffer (+ pos (length string))))))))

(defun buffer-current-package (buffer)
  "From point, search backwards for a top-level IN-PACKAGE form.
If no such form is found, then return the CL-USER package."
  ;; TODO: create a cache for this
  (let ((point (current-buffer *editor*))
        (temporary-package (make-package (gensym))))
    (import 'in-package temporary-package)
    (export 'in-package temporary-package)
    (unwind-protect
         (or (ignore-errors
               (save-excursion (buffer)
                  (let ((point (copy-mark (buffer-point buffer))))
                    (move-beginning-of-buffer buffer)
                    (let* ((str (buffer-string buffer (buffer-point buffer) point))
                           (pos (search (format nil "~A~A" #\( "in-package ") str :from-end t)))
                      (when (and pos (or (= 0 pos)
                                         (char= (char str (1- pos)) #\Newline)))
                        (let ((form (let ((*package* temporary-package)
                                          (*read-eval* nil))
                                      (ignore-errors
                                        (read-from-string (subseq str pos))))))
                          (when (and (listp form)
                                     (eql (first form) 'in-package)
                                     (= (list-length form) 2))
                            (return-from buffer-current-package (find-package (second form))))))))))
             (find-package :cl-user))
      (delete-package temporary-package))))

(defun eval-top-level-form-command ()
  (let ((buffer (current-buffer *editor*)))
    (save-excursion (buffer)
      (beginning-of-top-level-form buffer)
      (mark-to-point buffer (buffer-mark buffer))
      (move-sexp buffer 1)
      (let ((str (buffer-string buffer
                                (buffer-point buffer)
                                (buffer-mark buffer)))
            (package (buffer-current-package buffer)))
;        (format t "Read ~S in package ~S~%" str package)
        (let ((form (let ((*package* package))
                      (read-from-string str))))
          (format t "Evaluated ~S~%" (cadr form))
          (eval form))))))

(defun beginning-of-top-level-form-command ()
  (beginning-of-top-level-form (current-buffer *editor*)))

(defun newline-command ()
  (insert (current-buffer *editor*)  "
"))

(defun open-line-command ()
  (let ((buffer (current-buffer *editor*)))
    (move-end-of-line buffer)
    (newline-command)))

(defun eval-expression-command ()
  (format t "~A~%" (eval (read-from-string (read-from-minibuffer "Eval: ")))))

(defun copy-region-command ()
  (let ((buffer (current-buffer *editor*)))
    (copy-region buffer (buffer-point buffer) (buffer-mark buffer))))

(defun compile-buffer-command ()
  (let* ((buffer (current-buffer *editor*))
         (path (buffer-property buffer 'path)))
    (save-buffer-command)
    (format t "Background compiling ~A~%" path)
    (mezzano.supervisor::make-thread (lambda () (cal path))
       :name "MED Compilation"
       :initial-bindings `((*standard-output* ,(make-instance 'buffer-stream 
                                                              :buffer-name "*compilation*"))))))

(defun eval-last-sexp-command ()
   (let* ((buffer (current-buffer *editor*))
          (point (copy-mark (buffer-point buffer))))
     (save-excursion (buffer)
       (move-sexp buffer -1)
       (let ((string (buffer-string buffer point (buffer-point buffer))))
         (print (eval (read-from-string string)))))))

(defun find-matching-paren-command ()
  "Jump the cursor the paren that matches the one under the cursor."
  ;; FIXME: skip parens in strings
  (let* ((buffer (current-buffer *editor*))
         (point (copy-mark (buffer-point buffer)))
         (c (line-character (mark-line point) (mark-charpos point))))
    (when (char= c #\))
       (beginning-of-top-level-form buffer)
       (let ((string (buffer-string buffer point (buffer-point buffer)))
             (count 1))
         (do ((i (1- (length string)) (decf i)))
             ((< i 0))
            (unless (and (> i 1) (and (char= (char string (1- i)) #\\)
                                      (char= (char string (- i 2)) #\#)))
              (case (char string i)
                (#\( (decf count))
                (#\) (incf count))))
          (when (zerop count)
            (move-mark (buffer-point buffer) i)
            (return)))))
     (when (char= c #\()
       (beginning-of-top-level-form buffer)
       (move-sexp buffer)
       (let ((string (buffer-string buffer point (buffer-point buffer)))
             (count 0))
         (do ((i 0 (incf i)))
             ((= i (length string)))
            (unless (and (> i 1) (and (char= (char string (1- i)) #\\)
                                      (char= (char string (- i 2)) #\#)))
              (case (char string i)
                (#\( (incf count))
                (#\) (decf count))))
            (when (zerop count)
              (move-mark (buffer-point buffer) (- (length string)))
              (move-mark (buffer-point buffer) i)
              (return)))))))

(defun cancel-isearch ()
  (format t "Cancelling isearch.~%")
  (let ((buffer (current-buffer *editor*)))
    (setf (buffer-pre-command-hooks buffer)
          (remove 'isearch-pre-command-hook (buffer-pre-command-hooks buffer)))
    (setf (buffer-post-command-hooks buffer)
          (remove 'isearch-post-command-hook (buffer-post-command-hooks buffer)))))

(defun isearch-pre-command-hook ()
  (unless (or (eq *this-command* 'self-insert-command)
              (eq *this-command* 'isearch-command))
    (cancel-isearch)))
  
(defun isearch-post-command-hook ()
  (flet ((char-at-point (point)
           (line-character (mark-line point) (mark-charpos point))))
    (let* ((buffer (current-buffer *editor*))
           (point (buffer-point buffer)))
      (if (eql *this-command* 'self-insert-command)
        (progn
          (delete-backward-char-command)
          (if (= 0 (length *isearch-string*))           
            (progn
              (scan-forward point (lambda  (c) (char= c *this-character*)))
              (let ((char-at-point (char-at-point point)))
                (when (char= *this-character* char-at-point)
                  (vector-push-extend *this-character* *isearch-string*))))
            (let ((char-at-point (char-at-point point))
                  (next-char (progn (move-mark point 1)
                                    (character-right-of point)))) ;; FIXME: Hebrew
              (vector-push-extend *this-character* *isearch-string*)
              (unless (char= *this-character* char-at-point)
                (move-mark point -1)
                (search-forward buffer *isearch-string*)))))
        (if (null *isearch-string*)
          (setf *isearch-string* (make-array 0 :element-type 'character :adjustable t :fill-pointer t))
          (if (= 0 (length *isearch-string*))
            (search-forward buffer *last-isearch-string*) 
            (search-forward buffer *isearch-string*)))))))


(defun isearch-command ()
  (let ((buffer (current-buffer *editor*)))
    (unless (member 'isearch-post-command-hook (buffer-post-command-hooks buffer))
      (if (< 0 (length *isearch-string*))
        (setf *last-isearch-string* *isearch-string*))
      (format t "Starting isearch (Default: ~S)...~%" (coerce *last-isearch-string* 'string))
      (setf *isearch-string* nil)
      (push 'isearch-pre-command-hook (buffer-pre-command-hooks buffer))
      (push 'isearch-post-command-hook (buffer-post-command-hooks buffer)))))

(defun find-symbol-at-point-command ()
  (let* ((buffer (current-buffer *editor*))
         (symbol (symbol-at-point buffer)))
    (loop 
      (move-sexp buffer 1)
      (search-forward buffer symbol)
      (move-sexp buffer -1)
      (when (string= (symbol-at-point buffer) symbol)
          (return)))))

(defun execute-extended-command ()
  (let ((command (concatenate 'string "(med::" (read-from-minibuffer "M-x ") "-command)")))
    (format t "Executing extended command: ~A~%" command)
    (eval (read-from-string command))))

(defun new-frame-command ()
  (spawn))

(defun repl-command ()
  (start-repl))

(defun grep-command ()
  (grep))