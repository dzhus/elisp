(defun recode-directory (directory source target &optional pattern)
  "Recode files in DIRECTORY from SOURCE to TARGET encoding.

If PATTERN is non-nil, then only files with names matching it
will be processed."
  (interactive "DDirectory: \nzRead encoding: \nzWrite encoding: \nsFilename pattern: ")
  (dolist (file (directory-files-and-attributes directory t pattern))
    (when (not (elt file 1))
      (let ((filepath (first file)))
        (with-temp-buffer
          (let ((coding-system-for-read source))
            (insert-file-contents filepath))
          (let ((coding-system-for-write target))
            (write-file filepath)))))))
