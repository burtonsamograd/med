(in-package :med)

(defclass buffer-stream (sys.gray::fundamental-character-output-stream)
  ((buffer :initarg :buffer-name :reader buffer-stream-buffer-name)))

(defmethod sys.gray::stream-line-column ((stream buffer-stream))
  (col-index-of stream))

(defmethod sys.gray::stream-write-char ((stream buffer-stream) char)
  (let ((buffer (get-buffer-create (buffer-stream-buffer-name stream))))
    (save-excursion (buffer)
      (move-end-of-buffer buffer)
      (insert buffer char))))
