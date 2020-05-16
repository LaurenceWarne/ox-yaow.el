;;; ox-yaow.el --- Generate html pages from org files -*- lexical-binding: t -*-

;; Copyright (C) 2020 Laurence Warne

;; Author: Laurence Warne
;; Maintainer: Laurence Warne
;; Version: 0.1
;; Keywords: org
;; URL: https://github.com/LaurenceWarne/ox-yaow.el
;; Package-Requires: ((emacs "26") (f "0.2.0") (s "1.12.0") (dash "2.17.0"))

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

(defgroup ox-yaow nil
  "A lightweight wiki export option."
  :group 'ox)

(defcustom ox-yaow-headlline-point-to-file-p
  (lambda (hl) (= 2 (org-element-property :level hl)))
  "A function that returns t if some headline element points to an org file, else nil."
  :group 'ox-yaow
  :type 'function)

(defcustom ox-yaow-indexing-file-p
  (lambda (file-path) (let ((base (f-base file-path)))
			(or (string= base "index")
			    (string= (cadr (reverse (f-split file-path))) base))))
  "A function that returns t if the file should be treated as an indexing file, else nil."
  :group 'ox-yaow
  :type 'function)

(defcustom ox-yaow-get-default-indexing-file
  (lambda (file-path) (concat file-path
			      (when (not (char-equal ?/ (elt (reverse file-path) 0))) "/")
			      (f-base file-path) ".org"))
  "A function which takes the file path of a directory (which is known to have no indexing file) as an argument and returns the path of an indexing file which should be created for this directory."
  :group 'ox-yaow
  :type 'function)

(defvar ox-yaow-html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"https://fniessen.github.io/org-html-themes/styles/readtheorg/css/htmlize.css\"/><link rel=\"stylesheet\" type=\"text/css\" href=\"https://fniessen.github.io/org-html-themes/styles/readtheorg/css/readtheorg.css\"/><script src=\"https://ajax.googleapis.com/ajax/libs/jquery/2.1.3/jquery.min.js\"></script><script src=\"https://maxcdn.bootstrapcdn.com/bootstrap/3.3.4/js/bootstrap.min.js\"></script><script type=\"text/javascript\" src=\"https://fniessen.github.io/org-html-themes/styles/lib/js/jquery.stickytableheaders.min.js\"></script><script type=\"text/javascript\" src=\"https://fniessen.github.io/org-html-themes/styles/readtheorg/js/readtheorg.js\"></script>")

(cl-defun ox-yaow--get-file-ordering-from-index-tree (tree &key (headline-fun ox-yaow-headlline-point-to-file-p))
  "Get the ordering of files suggested by headlines collected from TREE."
  (let ((snd-level-headlines (org-element-map tree 'headline
			       (lambda (hl)
				 (and (funcall headline-fun hl) hl)))))
    ;; Get raw headline and convert to "emacs case"
    (--map (concat (downcase (s-replace " " "-" it)) ".org")
	   ;; Kill links
	   (--map (replace-regexp-in-string
		   ".*\\[\\[.+\\]\\[\\(.+\\)\\]\\].*" "\\1"
		   (org-element-property :raw-value it))
		  snd-level-headlines))))

(defun ox-yaow--get-file-ordering-from-index (indexing-file-path)
  (with-temp-buffer
    (insert-file-contents indexing-file-path)
    (ox-yaow--get-file-ordering-from-index-tree (org-element-parse-buffer))))

(cl-defun ox-yaow--get-html-relative-link
    (file-path link-text &key (reference-path file-path))
  "Return a HTML link element with text LINK-TEXT and href attribute equal to the path of FILE-PATH relative to REFERENCE-PATH, but with a .html extension."
  (let* ((relative-path (f-relative file-path (f-dirname reference-path)))
	 (html-path (concat (f-no-ext relative-path) ".html")))
    (concat "<a href='"
	    (when (not (char-equal ?. (elt html-path 0))) "./")
	    html-path "'>" link-text "</a>")))

(cl-defun ox-yaow--get-adjacent-strings (target-string strings &key (sort t))
  "Return a plist with property :preceding as the string preceding TARGET-STRING in STRINGS, and :succeeding as the string succeeding TARGET-STRING in STRINGS.  nil is used in either case if no such string exists.  If order is t, then sort STRINGS first.  nil is used in place of strings if such a string does not exist.  nil is returned if TARGET-STRING is not in STRINGS."
  (let* ((sorted-strings (if sort (sort strings #'string-lessp) strings))
	 (position (cl-position target-string strings :test #'string=)))
    (if position
	(let ((prev-idx (1- position))
	      (nxt-idx (1+ position)))
	  `(:preceding  ,(if (>= prev-idx 0) (nth prev-idx sorted-strings) nil)
	    :succeeding ,(if (< nxt-idx (length strings)) (nth nxt-idx sorted-strings) nil)))
      nil)))

(defun ox-yaow--get-nav-links (prev-file next-file)
  "Return a html string consisting of links to PREV-FILE and NEXT-FILE.  If one is nil it is ignored."
  (concat
   (when prev-file (concat "Previous: " (ox-yaow--get-html-relative-link
					 prev-file prev-file)))
   (when (and prev-file next-file) ", ")
   (when next-file (concat "Next: " (ox-yaow--get-html-relative-link
					 next-file next-file)))))

(cl-defun ox-yaow--get-up-file
    (target-file-path &key (indexing-file-p ox-yaow-indexing-file-p)
		      (get-default-indexing-file ox-yaow-get-default-indexing-file))
  "If TARGET-FILE-PATH points to an indexing file (determined by INDEXING-FILE-P), return the path to the indexing file in the parent directory of TARGET-FILE-PATH (or return the result of GET-DEFAULT-INDEXING-FILE if no such file exists).  Else return the file path of the indexing file in the same directory as TARGET-FILE-PATH."
  (let* ((directory (if (funcall indexing-file-p target-file-path)
		       (f-parent (f-parent target-file-path))
		     (f-parent target-file-path)))
	 (indexing-file (car (-filter indexing-file-p (f-files directory)))))
    (if indexing-file indexing-file (funcall get-default-indexing-file directory))))

(cl-defun ox-yaow--get-adjacent-files
    (target-file file-list &key (indexing-file-p ox-yaow-indexing-file-p))
  "Get files before and after TARGET-FILE in FILE-LIST.  These should all be full file paths.  INDEXING-FILE-P is a predicate determining whether a given file should be treated as an indexing file."
  (let* ((indexing-file (car (-filter indexing-file-p file-list)))
	 (file-ordering
	  (if indexing-file (ox-yaow--get-file-ordering-from-index indexing-file)
	    file-list)))
    (ox-yaow--get-adjacent-strings (f-base target-file)
				   (--map (f-base it) file-ordering)
				   :sort (unless indexing-file))))

(defun ox-yaow--get-up-file-link (up-file)
  "Return string html link to UP-FILE, or nil if UP-FILE is nil."
  (when up-file (concat ))
  )

(defun ox-yaow-template (contents info)
  "Transcode CONTENTS into yaow html format.  INFO is a plist used as a communication channel."
  (let* ((filename (f-base (plist-get info :input-file)))
	 (directory (f-dirname filename))
	 (org-paths-same-level (f-files directory (lambda (file) (s-suffix? "org" file))))
	 (adj-files (ox-yaow--get-adjacent-files filename org-paths-same-level))
	 (next-file (plist-get adj-files :succeeding))
	 (prev-file (plist-get adj-files :preceding))
	 (up-file (ox-yaow--get-up-file (plist-get info :input-file)))
	 (base-html (org-html-template contents info)))
    (print up-file)
    (replace-regexp-in-string "<h1"
			      (concat (ox-yaow--get-nav-links prev-file next-file)
				      (when (and up-file (or prev-file next-file))
					", Up: ")
				      (ox-yaow--get-html-relative-link
				       up-file (f-base up-file))
				      "<hr>"
				      "<h1")
			      base-html)))

;; Export options

(org-export-define-derived-backend 'ox-yaow-html 'html
  :options-alist `((:html-head "HTML_HEAD" nil ,ox-yaow-html-head newline))
  :menu-entry
  '(?h "Export to HTML"
       ((?w "As yaow wiki file" ox-yaow-org-export-to-html)))
  :translate-alist
  '((template . ox-yaow-template)))

(defun ox-yaow-org-export-to-html
    (&optional async subtreep visible-only body-only ext-plist)
  "Call org-export-to-file with the specified arguments.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer will then be accessible
through the org-export-stack interface.

Optional arguments SUBTREEP, VISIBLE-ONLY, BODY-ONLY and
EXT-PLIST are similar to those used in org-export-as, which
see."
  (let* ((extension ".html")
	 (file (org-export-output-file-name extension subtreep))
	 (org-export-coding-system 'utf-8))
    (org-export-to-file 'ox-yaow-html file
      async subtreep visible-only body-only ext-plist)))

(defun ox-yaow-publish-to-html (plist filename pub-dir)
  "Publish an Org file to a the ox-yaow backend.

BACKEND is a symbol representing the back-end used for
transcoding.  FILENAME is the filename of the Org file to be
published.  EXTENSION is the extension used for the output
string, with the leading dot.  PLIST is the property list for the
given project.

Optional argument PUB-DIR, when non-nil is the publishing
directory.

Return output file name."
  (org-publish-org-to #'ox-yaow-html filename ".html" plist pub-dir))

(provide 'ox-yaow)

;;; ox-yaow.el ends here
