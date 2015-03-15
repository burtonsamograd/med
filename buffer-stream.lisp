(in-package :med)

(defclass buffer-stream (sys.gray::fundamental-character-output-stream)
  ((buffer :initarg :buffer :reader buffer-stream-buffer)))

(defmethod sys.gray::stream-write-char ((stream buffer-stream) char)
  (let ((buffer (buffer-stream-buffer stream)))
    (save-excursion (buffer)
      (move-end-of-buffer buffer)
      (insert buffer char))
    (when (char= #\Newline char)
      (force-redisplay))))

