(in-package :med)

(defclass buffer-stream (sys.gray::fundamental-character-output-stream)
  ((buffer :initarg :buffer :reader buffer-stream-buffer)
   (filter :initarg :filter :reader buffer-stream-filter :initform nil)))

(defclass buffer-input-stream (sys.gray::fundamental-character-input-stream)
  ((buffer :initarg :buffer :reader buffer-stream-buffer)))

(defmethod sys.gray::stream-write-char ((stream buffer-stream) char)
  (let ((buffer (buffer-stream-buffer stream))
        (filter (buffer-stream-filter stream)))
    (move-end-of-buffer buffer)
    (insert buffer char)
    (let ((input-start (buffer-property buffer 'input-start)))
      (if input-start
          (move-mark-to-mark (buffer-property buffer 'input-start) (buffer-point buffer))
          (setf (buffer-property buffer 'input-start) (copy-mark (buffer-point buffer)))))
    (when filter
      (funcall filter buffer char))
    (when (or (char= char #\Newline) (char= char #\Space))
      (force-redisplay))))

(defmethod sys.gray::stream-read-char-no-hang ((stream buffer-stream))
  (let* ((buffer (buffer-stream-buffer stream))
         (point (buffer-point buffer))
         (input-start (buffer-property buffer 'input-start)))
    (when (mark> point input-start)
      (let* ((line (mark-line input-start))
             (c (handler-case (line-character line (mark-charpos input-start))
                (error () #\Newline))))
        (move-mark input-start)
        c))))
  
(defmethod sys.gray::stream-read-char ((stream buffer-stream))
  (loop
     (awhen (sys.gray::stream-read-char-no-hang stream)
       (return it))
     (mezzano.supervisor::fifo-push (mezzano.supervisor::fifo-pop (fifo *editor*))
                                    (fifo *editor*))))

(defmethod sys.gray::stream-unread-char ((stream buffer-stream) char)
  (let ((buffer (buffer-stream-buffer stream)))
    (move-mark (buffer-property buffer 'input-start) -1)))
