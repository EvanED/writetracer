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


(defun filter (lst pred)
  "Returns a list of the items in lst for which pred holds"
  (if (null lst)
      '()
    (let ((first (car lst))
	  (rest (filter (cdr lst) pred)))
      (if (funcall pred first)
	  (cons first rest)
	rest))))

(defun get-filename-from-chunk (chunk)
  "Given a chunk, returns what file it was written to"
  (cdr (assoc 'target_file chunk)))

(defun extract-chunks-for-file (trace filename)
  "Given a trace and filename, return a list of all chunks that wrote to that file"
  (filter trace 'get-filename-from-chunk))

(defun display-chunk (chunk)
  "Displays the given chunk at the appropriate offset (not correct yet) in the current buffer"
  (let ((end (cdr (assoc 'end_position chunk)))
	(size (cdr (assoc 'write_size chunk)))
	(contents (cdr (assoc 'contents chunk))))
    (if (equal end 0)
	;; then write at the end of the buffer
	(end-of-buffer)
      ;; otherwise write at end-size
      nil)
    (insert contents)))

(defun display-chunks-for-file (trace filename)
  (mapcar 'display-chunk (extract-chunks-for-file trace filename)))

(defun get-list-of-files (trace)
  (delete-dups (mapcar 'get-filename-from-chunk trace)))

(defun get-file-of-interest (files)
  (insert "Files that are referenced in the trace:\n")
  (mapcar '(lambda (f) (insert f) (insert "\n")) files)
  (let ((choice (read-string "What file would you like to trace: ")))
    (erase-buffer)
    choice))

(defun enter-trace-view-mode ()
  (interactive)
  (beginning-of-buffer)
  (let* ((trace (parse-buffer))
	 (files (get-list-of-files trace))
	 (subject (car files))
	 )
    (set (make-local-variable 'trace-view-orig-text) (buffer-string))
    (erase-buffer)
    (if (not (equal 1 (length files)))
	(setq subject (get-file-of-interest files))
      nil)
    (display-chunks-for-file trace subject)
    ))

(defun exit-trace-view-mode ()
  (interactive)
  (erase-buffer)
  (insert trace-view-orig-text))


