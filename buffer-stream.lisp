(in-package :med)

(defclass buffer-stream (sys.gray::fundamental-character-output-stream)
  ((buffer :initarg :buffer-name :reader buffer-stream-buffer-name)))

(defmethod sys.gray::stream-line-column ((stream buffer-stream))
  (col-index-of stream))

(defmethod sys.gray::stream-write-char ((stream buffer-stream) char)
  (insert (get-buffer-create (buffer-stream-buffer-name stream)) char))

(setf *messages* (make-instance 'buffer-stream :buffer-name "*Messages*"))
