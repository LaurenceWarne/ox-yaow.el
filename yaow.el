;;; yaow.el --- Generate html pages from org files -*- lexical-binding: t -*-

;; Copyright (C) 2020 Laurence Warne

;; Author: Laurence Warne
;; Maintainer: Laurence Warne
;; Version: 0.1
;; Keywords: org
;; URL: https://github.com/LaurenceWarne/yaow.el
;; Package-Requires: ((emacs "26") (f "0.2.0") (s "1.12.0"))

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
(require 's)
(require 'f)
(require 'dash)

(defvar yaow-html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"https://fniessen.github.io/org-html-themes/styles/readtheorg/css/htmlize.css\"/><link rel=\"stylesheet\" type=\"text/css\" href=\"https://fniessen.github.io/org-html-themes/styles/readtheorg/css/readtheorg.css\"/><script src=\"https://ajax.googleapis.com/ajax/libs/jquery/2.1.3/jquery.min.js\"></script><script src=\"https://maxcdn.bootstrapcdn.com/bootstrap/3.3.4/js/bootstrap.min.js\"></script><script type=\"text/javascript\" src=\"https://fniessen.github.io/org-html-themes/styles/lib/js/jquery.stickytableheaders.min.js\"></script><script type=\"text/javascript\" src=\"https://fniessen.github.io/org-html-themes/styles/readtheorg/js/readtheorg.js\"></script>")

(defun yaow--get-file-ordering-from-index-tree (tree)
  (let ((snd-level-headlines (org-element-map tree 'headline
			       (lambda (hl)
				 (and (= 2 (org-element-property :level hl)) hl)))))
    ;; Get raw headline and convert to "emacs case"
    (--map (concat (downcase (s-replace " " "-" (org-element-property :raw-value it)))
		   ".org")
	   snd-level-headlines)))

(defun yaow--get-file-ordering-from-index (indexing-file-path)
  (with-temp-buffer
    (insert-file-contents indexing-file-path)
    (yaow--get-file-ordering-from-index-tree (org-element-parse-buffer))))

(defun yaow--get-html-relative-link (org-file link-text)
  "Return a HTML link element with text LINK-TEXT which points to a file with the same name as ORG-FILE, but with a .html extension (ie a relative link)."
  (let* ((base (f-no-ext org-file))
	 (html-name (concat base ".html")))
    (concat "<a href='./" html-name "'>" link-text "</a>")))

(cl-defun yaow--get-adjacent-files (base-file files &key (sort t))
  "Return a cons cell whose car is the file from FILES preceding BASE-FILE, and whose cdr is the file from FILES succeeding BASE-FILE.  If order is t, then sort BASE-FILES first.  nil is used in place of files if such a file does not exist.  nil is returned if BASE-FILE is not in FILES."
  (let* ((sorted-files (if sort (sort files #'string-lessp) files))
	 (position (cl-position base-file files :test #'string=)))
    (if position
	(let ((prev-idx (1- position))
	      (nxt-idx (1+ position)))
	  `(,(if (>= prev-idx 0) (nth prev-idx sorted-files) nil) .
	    ,(if (< nxt-idx (length files)) (nth nxt-idx sorted-files) nil)))
      nil)))

(defun yaow-headline (headline contents info)
  "Transcode HEADLINE element into yaow html format.
CONTENTS is the headline contents.  INFO is a plist used as a
communication channel."
  (let ((level (org-export-get-relative-level headline info)))
    (if (= level 1)
	(let* ((directory (f-dirname (plist-get info :input-file)))
	       (org-files-same-level
		(f-files directory (lambda (file) (s-suffix? "org" file))))
	       (base-html (org-html-headline headline contents info)))
	  (message (format "files in same directory: %s"
			   org-files-same-level))
	  (message base-html)
	  (replace-regexp-in-string "</h2>" "</h2><small>Hi!</small>" base-html))
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
