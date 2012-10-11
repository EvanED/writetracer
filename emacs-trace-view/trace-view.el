(defun parse-buffer ()
  "Assumes the current buffer holds a sequence of 'read-able Lisp objects. Returns a list of them."
  (beginning-of-buffer)
  (let ((trace '()))
    ;; Read each item in the buffer until end-of-file,
    ;; prepending them to the list 'trace
    (condition-case nil
	(while t
	  (setq trace (cons (read (current-buffer)) trace)))
      (end-of-file 5))

    ;; Then return the trace in proper order
    (reverse trace)))

(with-current-buffer (get-buffer ".trace")
  (parse-buffer)
)


