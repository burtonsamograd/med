(in-package :med)

(defvar *repl-key-map*)
(defvar *repl-history* '())
(defvar *repl-history-number* 0)

(defun repl-prompt (buffer)
   (insert buffer (format nil "~A> " (package-name *package*)))
   (setf (buffer-property buffer 'repl-prompt-end) (copy-mark (buffer-point buffer))))

(defun start-repl ()
  (initialize-repl-key-map)
  (let ((buffer (get-buffer "*repl*")))
    (unless buffer
      (setf buffer (make-instance 'buffer
                                  :key-map *repl-key-map*)
            (buffer-property buffer 'name) "*repl*"
            (last-buffer *editor*) (current-buffer *editor*))
      (repl-prompt buffer)
      (push buffer (buffer-list)))
  (setf (last-buffer *editor*) (current-buffer *editor*))
  (switch-to-buffer buffer)))

(defun repl-finish-input-command ()
  (let ((buffer (current-buffer *editor*)))
    (move-end-of-line buffer)
    ;; FIXME: clearing the buffer by cutting the text causes the 
    ;; editor to crash when you hit enter
    (let* ((code (buffer-string buffer
                                (buffer-property buffer 'repl-prompt-end)
                                (buffer-point buffer)))
           (output (if (string= code "")
                     ""
                     (progn 
                       (with-output-to-string (s)
                         (format s "~A~%" 
                                 (handler-case
                                   (eval (read-from-string code))
                                 (error (e) (format s "~A~%" e) ""))))))))
      (push code *repl-history*)
      (insert buffer #\Newline)
      (insert buffer output))
      (repl-prompt buffer)
      (setf *repl-history-number* 0)))

(defun repl-clear-output ()
  (let ((buffer (current-buffer *editor*)))
    (move-end-of-buffer buffer)
    (let ((point (copy-mark (buffer-point buffer))))
      (move-beginning-of-buffer buffer)
      (delete-region buffer point (buffer-point buffer)))
    (repl-prompt buffer)))

(defun repl-delete-input ()
  (let ((buffer (current-buffer *editor*)))
    (move-end-of-buffer buffer)
    (delete-region buffer
                   (buffer-property buffer 'repl-prompt-end) 
                   (buffer-point buffer))))

(defun repl-previous-history ()
  (when (< *repl-history-number* (length *repl-history*))
    (repl-delete-input)
    (insert (current-buffer *editor*) (nth *repl-history-number* *repl-history*))
    (incf *repl-history-number*)))

(defun repl-next-history ()
  (when (>= *repl-history-number* 0)
    (repl-delete-input)
    (insert (current-buffer *editor*) (nth *repl-history-number* *repl-history*))
    (decf *repl-history-number*)))

(defun repl-beginning-of-line ()
  (let ((buffer (current-buffer *editor*)))
    (move-mark-to-mark (buffer-point buffer) (buffer-property buffer 'repl-prompt-end))))

(defvar *repl-complete-results* ())
(defvar *repl-complete-results-number* 0)
(defun repl-complete ()
  (let ((buffer (current-buffer *editor*)))
  (if (and (eql *last-command* 'repl-complete) *repl-complete-results*)
    (progn
      (format t "here")
      (let ((point (copy-mark (buffer-point buffer))))
        (move-sexp buffer -1)
        (delete-region buffer point (buffer-point buffer))
        (insert buffer (nth *repl-complete-results-number* *repl-complete-results*))
        (setf *repl-complete-results-number* 
              (mod (1+ *repl-complete-results-number*) (length *repl-complete-results*)))))
    (progn
  (let ((symbol (symbol-at-point buffer))
        results)
    (push symbol results)
    (do-symbols (s) 
       (when (eql 0 (search symbol (symbol-name s) :test #'string-equal))
         (push (string-downcase (symbol-name s)) results)))
    (format t "~A" results)
    (let ((point (copy-mark (buffer-point buffer))))
       (move-sexp buffer -1)
       (delete-region buffer point (buffer-point buffer))
       (insert buffer (nth 0 results))
       (setf *repl-complete-results* results)
       (setf *repl-complete-results-number* 0)))))))

(defun initialize-repl-key-map ()
  (setf *repl-key-map* (make-hash-table))
  (set-key #\C-M 'repl-finish-input-command *repl-key-map*)
  (set-key #\Newline 'repl-finish-input-command *repl-key-map*)
  (set-key '(#\C-C #\M-O) 'repl-clear-output *repl-key-map*)
  (set-key #\M-P 'repl-previous-history *repl-key-map*)
  (set-key #\M-N 'repl-next-history *repl-key-map*)
  (set-key #\C-A 'repl-beginning-of-line *repl-key-map*)
  (set-key #\Tab 'repl-complete *repl-key-map*)
)