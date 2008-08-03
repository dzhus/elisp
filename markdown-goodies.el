;;; markdown-goodies.el -- additional features for Emacs markdown-mode

;; Copyright (C) 2007, 2008 Dmitry Dzhus

;; Author: Dmitry Dzhus <mail@sphinx.net.ru>

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package adds an additional `markdown-check-refs` function
;; (also available using `C-c C-c c` keybinding) to markdown-mode.
;; Call it interactively to see a list of all [undefined][references]
;; in your text in a separate buffer. Hitting a reference name in a
;; new buffer creates an empty reference definition in the end of your
;; document, hitting line number (listed in parentheses after
;; reference name) moves you to the line reference occurs in the text.

;; Install this by writing `(load "/path/to/markdown-goodies.el")`
;; somewhere in your Emacs initialization file.

;; ATTENTION: As of June 2008, reference checking functions are
;; included in markdown-mode itself. Please DO NOT use this separate
;; package with markdown-mode 1.6 or greater.


(require 'markdown-mode)

;;; Code:
(defconst markdown-refcheck-buffer "*Undefined references for %BUFFER%*"
  "Name of buffer which will contain a list of undefined references in `markdown-mode' buffer named %BUFFER%.")

;;;; Kosher version
(defun markdown-has-reference-definition (reference)
    "Find out whether Markdown REFERENCE is defined.

REFERENCE includes square brackets, like [this]."
    (let ((reference (downcase reference)))
      (save-excursion
        (goto-char (point-min))
        (catch 'found
          (while (re-search-forward regex-reference-definition nil t)
            (when (string= reference (downcase (match-string-no-properties 1)))
              (throw 'found t)))))))

(defun markdown-get-undefined-refs ()
  "Return a list of undefined Markdown references.

Result is an alist of pairs (reference . occurencies), where
occurencies is itself another alist of pairs (label .
line-number).

For example, an alist corresponding to [Nice editor][Emacs] at line 12,
\[GNU Emacs][Emacs] at line 45 and [manual][elisp] at line 127 is
\((\"[emacs]\" (\"[Nice editor]\" . 12) (\"[GNU Emacs]\" . 45)) (\"[elisp]\" (\"[manual]\" . 127)))."
  (let ((missing))
    (save-excursion
      (goto-char (point-min))
      (while
          (re-search-forward regex-link-reference nil t)
        (let* ((label (match-string-no-properties 1))
               (reference (match-string-no-properties 2))
               (target (downcase (if (string= reference "[]") label reference))))
          (unless (markdown-has-reference-definition target)
            (let ((entry (assoc target missing)))
              (if (not entry)
                  (add-to-list 'missing (cons target
                                              (list (cons label (line-number-at-pos)))) t)
                (setcdr entry
                        (append (cdr entry) (list (cons label (line-number-at-pos))))))))))
      missing)))

(defun markdown-add-missing-ref-definition (ref buffer &optional recheck)
  "Add blank REF definition to the end of BUFFER.

REF is a Markdown reference in square brackets, like \"[lisp-history]\".

When RECHECK is non-nil, BUFFER gets rechecked for undefined
references so that REF disappears from the list of those links."
  (with-current-buffer buffer
      (when (not (eq major-mode 'markdown-mode))
        (error "Not available in current mdoe"))
      (goto-char (point-max))
      (indent-new-comment-line)
      (insert (concat ref ": ")))
  (switch-to-buffer-other-window buffer)
  (goto-char (point-max))
  (when recheck
    (markdown-check-refs t)))

;; Button which adds an empty Markdown reference definition to the end
;; of buffer specified as its 'target-buffer property. Reference name
;; is button's label
(define-button-type 'markdown-ref-button
  'help-echo "Push to create an empty reference definition"
  'face 'bold
  'action (lambda (b)
            (markdown-add-missing-ref-definition
             (button-label b) (button-get b 'target-buffer) t)))

;; Button jumping to line in buffer specified as its 'target-buffer
;; property. Line number is button's 'line property.
(define-button-type 'goto-line-button
  'help-echo "Push to go to this line"
  'face 'italic
  'action (lambda (b)
            (message (button-get b 'buffer))
            (switch-to-buffer-other-window (button-get b 'target-buffer))
            (goto-line (button-get b 'target-line))))

(defun markdown-check-refs (&optional silent)
  "Show all undefined Markdown references in current `markdown-mode' buffer.

If SILENT is non-nil, do not message anything when no undefined
references found.

Links which have empty reference definitions are considered to be
defined."
  (interactive "P")
  (when (not (eq major-mode 'markdown-mode))
    (error "Not available in current mode"))
  (let ((oldbuf (current-buffer))
        (refs (markdown-get-undefined-refs))
        (refbuf (get-buffer-create (replace-regexp-in-string
                                 "%BUFFER%" (buffer-name)
                                 markdown-refcheck-buffer t))))
    (if (null refs)
        (progn
          (when (not silent)
            (message "No undefined references found"))
          (kill-buffer refbuf))
      (with-current-buffer refbuf
        (when view-mode
          (View-exit-and-edit))
        (erase-buffer)
        (insert "Following references lack definitions:")
        (newline 2)
        (dolist (ref refs)
          (let ((button-label (format "%s" (car ref))))
            (insert-text-button button-label
                                :type 'markdown-ref-button
                                'target-buffer oldbuf)
            (insert " (")
            (dolist (occurency (cdr ref))
              (let ((line (cdr occurency)))
                (insert-button (number-to-string line)
                               :type 'goto-line-button
                               'target-buffer oldbuf
                               'target-line line)
                (insert " "))) (delete-backward-char 1)
                (insert ")")
                (newline))))
      (view-buffer-other-window refbuf)
      (goto-line 4))))

(define-key markdown-mode-map "\C-c\C-cc" 'markdown-check-refs)
(provide 'markdown-goodies)

;;; markdown-goodies.el ends here
