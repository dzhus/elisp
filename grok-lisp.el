;;; grok-lisp.el --- Extract or graph sources with Emacs Semantic

;; Copyright (C) 2007, 2008 Dmitry Dzhus

;; Author: Dmitry Dzhus <mail@sphinx.net.ru>
;; Keywords: lisp semantic graph

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

;; This code is intended to be used in Emacs batch mode. It allows you
;; to print contents of Semantic tag in file to standard output and to
;; print dependency DOT graph of Semantic tags in a list of files. See
;; article at http://sphinx.net.ru/blog/entry/semantic-wizardry/ (in
;; russian).

;; The following command prints source depgraph to standard output:
;; 
;;     emacs --batch -l grok-lisp.el --exec "(print-files-depgraph '(function) \"some-file.el\")" 2> /dev/null
;;
;; This is for printing contents of arbitary tag in arbitary source file:
;;
;;     emacs --batch --l grok-lisp.el --exec '(print-tag-from-file "some-tag" "some-source.scm")' 2> /dev/null

;; This package has been tested with Emacs Lisp and Scheme sources. It
;; contains a Lisp specific hack in `get-tag-deps', you may need to
;; tweak it in order to parse sources in other languages.

;;; Code:

(require 'semantic)
(require 'semanticdb)
(semanticdb-toggle-global-mode)

(defun get-file-tags (file-name)
  "Return a Semantic tag table in FILE-NAME."
  (with-current-buffer
      (find-file-noselect file-name)
    (semantic-fetch-tags)))

(defun get-tag-body (tag)
  "Return full TAG source code as a string.

TAG is valid Semantic tag."
  (let ((from (semantic-tag-start tag))
        (to (semantic-tag-end tag))
        (buffer (semantic-tag-buffer tag)))
    (with-current-buffer buffer
      (buffer-substring from to))))

(defun get-tag-deps (tag tag-table types)
  "Scan TAG for tags which are also in TAG-TABLE.

Only tokens with one of specified TYPES are considered to be a
tag dependency."
(let ((from (semantic-tag-start tag))
        (to (semantic-tag-end tag))
        (buffer (semantic-tag-buffer tag))
        ;; Build associative list with tag names as keys
        (deps (mapcar
               (lambda (tag)
                 (cons (semantic-tag-name tag)
                       tag))
               tag-table)))
    (with-current-buffer buffer
      (let (result)
        ;; cddddr is a Lisp-oriented hack to prevent tag itself from
        ;; inclusion to dependency list
        (dolist (lexem (cddddr (semantic-lex from to 1.0e+INF)) result)
          (if (memq (car lexem) types)
              (let* ((lexem-string (buffer-substring
                                    (cadr lexem)
                                    (cddr lexem)))
                     (found-tag (assoc lexem-string
                                       deps)))
                (if found-tag
                    (add-to-list 'result (cdr found-tag) t)))))))))

(defun print-tag-from-file (tag-name file-name)
  "Print body of tag with TAG-NAME from FILE-NAME."
(interactive "sTag name: \nfFile name: ")
  (let ((tag-table (get-file-tags file-name)))
    (princ (format "%s"
                   (get-tag-body
                    (semantic-find-first-tag-by-name
                     tag-name
                     tag-table))))))

(defun get-file-tag-classes (file-name classes)
  "Get a list of all tags declared in FILE-NAME which class is in CLASSES.

CLASSES is a list of Semantic classes."
  (let ((result)
        (all-tags (get-file-tags file-name)))
    (dolist (tag-class classes result)
      (setq result (append result
                           (semantic-find-tags-by-class
                            tag-class all-tags))))))

(defun get-file-tag-classes-deep (file-name classes)
  "List all tags declared in FILE-NAME and includes which class is in CLASSES.

CLASSES is a list of Semantic classes.

Does the same as `get-file-tag-classes' taking included files
into account."
  (with-current-buffer
      (find-file-noselect file-name)
    (let ((result))
      (dolist (tag-class classes result)
        (setq result (append result (semanticdb-strip-find-results
                                     (semanticdb-find-tags-by-class tag-class))))))))

(defun get-file-depgraph (file-name classes)
  "Extract from FILE-NAME tags of CLASSES with their dependencies.

CLASSES is a list of Semantic classes.

Return a list of pairs (TAG . DEPS) where DEPS is a list of
functions TAG depends on."
(let ((deep-tag-table (get-file-tag-classes-deep file-name classes))
        (file-tag-table (get-file-tag-classes file-name classes))
        (depgraph))
    (dolist (tag file-tag-table depgraph)
      (let ((deps (get-tag-deps tag deep-tag-table '(NAME symbol))))
        (add-to-list 'depgraph (cons tag deps) t)))))

(defun print-files-depgraph (classes &rest file-names)
  "Print depgraph with tags of CLASSES found in FILE-NAMES.

CLASSES is a list of Semantic classes (description of Semantic
classes is available in CEDET manual). Usually you'll use
`'(function)'.

FILE-NAMES are source code files to search for tags in.

Output format is DOT, so it's suitable for further processing
with Graphviz tools."
(princ "digraph D {\n")
  (princ "overlap=scale;\n")
  (dolist (file file-names)
    (let ((depgraph (get-file-depgraph file classes)))
      (dolist (dep-list-for-tag depgraph)
        (let ((function-name (semantic-tag-name
                              (car dep-list-for-tag))))
          (princ (format "\"%s\";\n" function-name))
          (dolist (dependency (cdr dep-list-for-tag))
            (princ (format "\"%s\" -> \"%s\";\n"
                           (semantic-tag-name dependency)
                           function-name)))))))
  (princ "}\n"))

(provide 'grok-lisp)

;;; grok-lisp.el ends here
