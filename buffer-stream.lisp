(in-package :med)

(defclass buffer-stream (sys.gray::fundamental-character-output-stream)
  ((buffer :initarg :buffer :reader buffer-stream-buffer)
   (filter :initarg :filter :reader buffer-stream-filter :initform nil)))

(defclass buffer-input-stream (sys.gray::fundamental-character-input-stream)
  ((buffer :initarg :buffer :reader buffer-stream-buffer)))

(defmethod sys.gray::stream-write-char ((stream buffer-stream) char)
  (let ((buffer (buffer-stream-buffer stream))
        (filter (buffer-stream-filter stream)))
    (save-excursion (buffer)
      (move-end-of-buffer buffer)
      (insert buffer char)
      (when (buffer-property buffer 'input-start)
        (delete-mark (buffer-property buffer 'input-start)))
      (setf (buffer-property buffer 'input-start) (copy-mark (buffer-point buffer))))
    (when filter
      (funcall filter buffer char))
    (when (char= #\Newline char)
      (force-redisplay))))

(defmethod sys.gray::stream-read-char ((stream buffer-stream))
  (loop
  (let* ((buffer (buffer-stream-buffer stream))
         (point (buffer-point buffer))
         (input-start (buffer-property buffer 'input-start)))
    (ignore-errors
    (when (mark> point input-start)
      (let* ((line (mark-line input-start))
             (c (if (end-of-line-p line) #\Newline
                    (line-character line (mark-charpos input-start)))))
        (move-mark-to-mark point input-start)
        (return c)))))
   (mezzano.supervisor::fifo-push (mezzano.supervisor::fifo-pop (fifo *editor*))
                                  (fifo *editor*))))

(defmethod sys.gray::stream-unread-char ((stream buffer-stream) char)
  (let ((buffer (buffer-stream-buffer stream)))
    (save-excursion (buffer)
      (move-end-of-buffer buffer)
      (insert buffer char)
      (when (buffer-property buffer 'input-start)
        (delete-mark (buffer-property buffer 'input-start)))
      (setf (buffer-property buffer 'input-start) (copy-mark (buffer-point buffer))))
    (when (char= #\Newline char)
      (force-redisplay))))
