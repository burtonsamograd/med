(in-package :med)

(defclass buffer-stream (sys.gray::fundamental-character-output-stream)
  ((buffer :initarg :buffer :reader buffer-stream-buffer)))

(defmethod sys.gray::stream-line-column ((stream buffer-stream))
  (sys.gray::col-index-of stream))

(defmethod sys.gray::stream-write-char ((stream buffer-stream) char)
  (let ((buffer (buffer-stream-buffer stream)))
    (save-excursion (buffer)
      (move-end-of-buffer buffer)
      (insert buffer char))))
