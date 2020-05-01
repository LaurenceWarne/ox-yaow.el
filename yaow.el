;;; yaow.el --- Generate html pages from org files -*- lexical-binding: t -*-

;; Copyright (C) 2020 Laurence Warne

;; Author: Laurence Warne
;; Maintainer: Laurence Warne
;; Version: 0.1
;; Keywords: org
;; URL: https://github.com/LaurenceWarne/yaow.el
;; Package-Requires: ((emacs "26"))

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

;;; Commentary:
;;
;; This package adds another org html export option which aims to provide a
;; lightweight wiki.

;;; Code:

(require 'ox)
(require 'ox-html)

(defvar yaow-html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"https://fniessen.github.io/org-html-themes/styles/readtheorg/css/htmlize.css\"/><link rel=\"stylesheet\" type=\"text/css\" href=\"https://fniessen.github.io/org-html-themes/styles/readtheorg/css/readtheorg.css\"/><script src=\"https://ajax.googleapis.com/ajax/libs/jquery/2.1.3/jquery.min.js\"></script><script src=\"https://maxcdn.bootstrapcdn.com/bootstrap/3.3.4/js/bootstrap.min.js\"></script><script type=\"text/javascript\" src=\"https://fniessen.github.io/org-html-themes/styles/lib/js/jquery.stickytableheaders.min.js\"></script><script type=\"text/javascript\" src=\"https://fniessen.github.io/org-html-themes/styles/readtheorg/js/readtheorg.js\"></script>")

(defun yaow-headline (headline contents info)
  "Transcode HEADLINE element into yaow html format.
CONTENTS is the headline contents.  INFO is a plist used as a
communication channel."
  (let ((level (org-export-get-relative-level headline info)))
    (if (= level 1)
	(progn
	  (message (format "Headline: %s, level: %s"
			   (org-element-property :raw-value headline);(plist-get info :input-file)
			   level))
	  (message (org-html-headline headline contents info)))
      (org-html-headline headline contents info))))

(defun yaow-org-export-to-html
    (&optional async subtreep visible-only body-only ext-plist)
  "Org export menu entry for read-the-org."
  (let* ((extension ".html")
	 (file (org-export-output-file-name extension subtreep))
	 (org-export-coding-system 'utf-8))
    (org-export-to-file 'yaow-html file
      async subtreep visible-only body-only ext-plist)))

(org-export-define-derived-backend 'yaow-html 'html
  :options-alist `((:html-head "HTML_HEAD" nil ,yaow-html-head newline))
  :menu-entry
  '(?h "Export to HTML"
       ((?w "As yaow wiki file" yaow-org-export-to-html)))
  :translate-alist
  '((headline . yaow-headline)))


(provide 'yaow)

;;; yaow.el ends here
