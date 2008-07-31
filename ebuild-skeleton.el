;;; ebuild-skeleton.el --- skeleton for new Gentoo ebuilds

;; Copyright (C) 2008  Dmitry Dzhus

;; Author: Dmitry Dzhus <dima@sphinx.net.ru>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(defun ebuild-read-license (prompt)
  "Read a license name from the minibuffer.

PROMPT is used as minibuffer prompt, input is completed it to one
of licenses in Portage if possible."
  (completing-read prompt
   (file-name-all-completions "" "/usr/portage/licenses")
   nil
   nil
   "GPL-2"))

(define-skeleton ebuild-skeleton
  "Insert new ebuild skeleton.

Ask for package description, homepage, source URL, license, USE
flags and dependencies interactively."
  nil
  '(insert-file-contents "/usr/portage/header.txt")
  '(goto-char (point-max))
  "inherit eutils" \n \n
  "DESCRIPTION=\"" (read-string "Enter description: ") "\"" \n
  "HOMEPAGE=\"" (read-string "Enter homepage URL: " "http://") "\"" \n
  "SRC_URI=\"" (read-string "Enter source code URL: " "http://") "\"" \n
  "LICENSE=\"" (ebuild-read-license "Enter package license: ") "\"" \n \n
  "SLOT=\"0\"" \n
  "KEYWORDS=\"~x86\"" \n
  "IUSE=\"" (read-string "Enter USE flags: ") "\"" \n \n
  "DEPEND=\"" ("Enter dependency atom: " str & (nil ?\n "	")) & (if skeleton-untabify -5 -2) "\"" ?\n
  "RDEPEND=\"${DEPEND}\"" \n \n
  "src_unpack() {" \n _ "unpack ${A}" > ?\n "}" \n \n
  "src_compile() {" \n "emake || die \"emake failed\"" > ?\n "}" \n \n
  "src_install() {" \n "emake DESTDIR=\"${D}\" install || die \"Install failed\"" > ?\n "}")

(provide 'ebuild-skeleton)
;;; ebuild-skeleton.el ends here
