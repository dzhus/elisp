(define-skeleton ebuild-skeleton
  "New ebuild skeleton"
  ""
  '(insert-file "/usr/portage/header.txt")
  '(goto-char (point-max))
  "DESCRIPTION=\"" (skeleton-read "Enter description: ") "\"" \n
  "HOMEPAGE=\"" (skeleton-read "Enter homepage URL: ") "\"" \n
  "SRC_URI=\"" (skeleton-read "Enter source code URL: ") "\"" \n
  "LICENSE=\"" (skeleton-read "Enter package license: " "GPL-2") "\"" \n \n
  "SLOT=\"0\"" \n
  "KEYWORDS=\"~x86\"" \n
  "IUSE=\"\"" \n \n
  "DEPEND=\"" ("Enter dependency atom: " str ?\n "	") -2 "\"" \n
  "RDEPEND=\"${DEPEND}\""
  )