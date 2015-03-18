(in-package :med)

(defun list-buffers-command ()
  (let ((buffer (get-buffer-create "*Buffers*")))
    (setf (last-buffer *editor*) (current-buffer *editor*))    
    (switch-to-buffer buffer)
    ;; Clear the whole buffer.
    (delete-region buffer
                   (make-mark (first-line buffer) 0)
                   (make-mark (last-line buffer) (line-length (last-line buffer))))
    (dolist (b (buffer-list))
      (insert buffer (buffer-property b 'name))
      (insert buffer #\Newline))
    (setf (buffer-modified buffer) nil)))

(defun buffer-completer (text)
  (let (results)
    (push text results)
    (dolist (buffer *buffer-list*)
      (when (search text (buffer-property buffer 'name))
        (push (buffer-property buffer 'name) results)))
    results))

(defun switch-to-buffer-command ()
  (let* ((default-buffer (or (last-buffer *editor*)
                             (current-buffer *editor*)))
         (name (string-trim " " (read-from-minibuffer (format nil "Buffer (default ~A): " (buffer-property default-buffer 'name)) :completer #'buffer-completer)))
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
         (get-buffer-create "*Scratch*")))
    (when (>= (length (buffer-list)) 2)
       (setf (last-buffer *editor*) (second (buffer-list))))))

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
