(in-package :med)

(defvar *repl-key-map*)
(defvar *repl-history* '())
(defvar *repl-history-number* 0)

(defun repl-prompt (buffer)
   (move-end-of-buffer buffer)
   (insert buffer (format nil "~A> " (sys.int::package-shortest-name *package*)))
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

(defun repl-eval (code)
  (if (string= code "")
       ""
       (let ((s (make-instance 'buffer-stream :buffer (get-buffer "*repl*"))))
         (handler-case
           (let ((*standard-output* s))
             (format s "~%~S" (eval (read-from-string code))))
             (error (e) (format s "~%~S~%" e) "")))))

(defun repl-finish-input-command ()
  (let ((buffer (current-buffer *editor*)))
    (move-end-of-line buffer)
    ;; FIXME: clearing the buffer by cutting the text causes the 
    ;; editor to crash when you hit enter
    (let ((code (buffer-string buffer
                               (buffer-property buffer 'repl-prompt-end)
                               (buffer-point buffer))))
      (repl-eval code)
      (when (and (> (length code) 0)
                 (not (string= code (car *repl-history*))))
        (push code *repl-history*))
      (insert buffer #\Newline)
      (repl-prompt buffer)
      (setf *repl-history-number* 0))))

(defun repl-clear-output ()
  (let ((buffer (current-buffer *editor*)))
    (move-end-of-buffer buffer)
    (with-mark (point (buffer-point buffer))
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
    (with-mark (point (buffer-point buffer))
        (move-sexp buffer -1)
        (delete-region buffer point (buffer-point buffer))
        (insert buffer (nth *repl-complete-results-number* *repl-complete-results*))
        (setf *repl-complete-results-number* 
              (mod (1+ *repl-complete-results-number*) (length *repl-complete-results*)))))
    (progn
  (let ((symbol (symbol-at-point buffer))
        results)
    (push symbol results)
    (setf symbol (string-upcase symbol))
    (do-symbols (s) 
       (when (eql 0 (search symbol (symbol-name s) :test #'equal))
         (push (string-downcase (symbol-name s)) results)))
    ;;(format t "~A" results)
    (with-mark (point (buffer-point buffer))
       (move-sexp buffer -1)
       (delete-region buffer point (buffer-point buffer))
       (insert buffer (nth 0 results))
       (setf *repl-complete-results* results)
       (setf *repl-complete-results-number* 0)))))))

;; TODO: refactor this with find-matching-paren-command
;; make a generic function that takes a function to move to the 
;; beginning of a toplevel form
(defun repl-find-matching-paren ()
  "Jump the cursor the paren that matches the one under the cursor."
  ;; FIXME: skip parens in strings
  (with-mark (point (current-bufer *editor*))
    (let* ((buffer (current-buffer *editor*))
           (c (line-character (mark-line point) (mark-charpos point))))
      (when (char= c #\))
         (repl-beginning-of-line)
         (let ((string (buffer-string buffer 
                                      point
                                      (buffer-point buffer)))
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
         (repl-beginning-of-line)
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
                (return))))))))

(defun initialize-repl-key-map ()
  (setf *repl-key-map* (make-hash-table))
  (set-key #\C-M 'repl-finish-input-command *repl-key-map*)
  (set-key #\Newline 'repl-finish-input-command *repl-key-map*)
  (set-key '(#\C-C #\M-O) 'repl-clear-output *repl-key-map*)
  (set-key #\M-P 'repl-previous-history *repl-key-map*)
  (set-key #\M-N 'repl-next-history *repl-key-map*)
  (set-key #\C-A 'repl-beginning-of-line *repl-key-map*)
  (set-key #\Tab 'repl-complete *repl-key-map*)
  (set-key #\M-O 'repl-find-matching-paren *repl-key-map*)  
)
