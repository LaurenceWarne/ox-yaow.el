;;; ox-yaow.el --- Generate html pages from org files -*- lexical-binding: t -*-

;; Copyright (C) 2020 Laurence Warne

;; Author: Laurence Warne
;; Maintainer: Laurence Warne
;; Version: 0.1
;; Keywords: org
;; URL: https://github.com/LaurenceWarne/ox-yaow.el
;; Package-Requires: ((emacs "27") (f "0.2.0") (s "1.12.0") (dash "2.17.0"))

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
  #'ox-yaow--indexing-file-p
  "A function that returns t if the file should be treated as an indexing file, else nil."
  :group 'ox-yaow
  :type 'function)

(defcustom ox-yaow-get-default-indexing-file
  #'ox-yaow--get-default-indexing-file
  "A function which takes the file path of a directory (which is known to have no indexing file) as an argument and returns the path of an indexing file which should be created for this directory."
  :group 'ox-yaow
  :type 'function)

(defcustom ox-yaow-html-link-stitching-fn
  #'ox-yaow--html-link-stitching-fn
  "A function that takes three strings (file-paths) as arguments, denoting the \"previous file\", \"next file\" and \"parent file\" respectively (any of which may be nil), and returns valid html corresponding to linking to these files."
  :group 'ox-yaow
  ;; TODO can I put function of three args here?
  :type 'function)

(defcustom ox-yaow-wiki-home-filename
  "wiki"
  "Name of the file to serve as the home page of the wiki."
  :group 'ox-yaow
  :type 'string)

(defvar ox-yaow-html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"https://fniessen.github.io/org-html-themes/styles/readtheorg/css/htmlize.css\"/><link rel=\"stylesheet\" type=\"text/css\" href=\"https://fniessen.github.io/org-html-themes/styles/readtheorg/css/readtheorg.css\"/><script src=\"https://ajax.googleapis.com/ajax/libs/jquery/2.1.3/jquery.min.js\"></script><script src=\"https://maxcdn.bootstrapcdn.com/bootstrap/3.3.4/js/bootstrap.min.js\"></script><script type=\"text/javascript\" src=\"https://fniessen.github.io/org-html-themes/styles/lib/js/jquery.stickytableheaders.min.js\"></script><script type=\"text/javascript\" src=\"https://fniessen.github.io/org-html-themes/styles/readtheorg/js/readtheorg.js\"></script>")

(defvar ox-yaow--generated-files nil)

(defun ox-yaow--org-assoc-file-p (path)
  "Return t if PATH is an org file, or is a directory which is an ancestor of one, else return nil."
  (or (s-ends-with-p ".org" path)
      (and (f-directory? path)
           (-any #'ox-yaow--org-assoc-file-p (f-entries path)))))

(defun ox-yaow--org-entries (path &optional ignore-indexing-files)
  "Return a list of org files and directories containing org files resident in PATH, if IGNORE-INDEXING-FILES is non-nil, skip indexing files."
  (-filter (lambda (path) (or (not ignore-indexing-files)
			      (not (funcall ox-yaow-indexing-file-p path))))
	   (-filter #'ox-yaow--org-assoc-file-p (f-entries path))))

(cl-defun ox-yaow--indexing-file-p
    (file-path &key (strings (list "index" ox-yaow-wiki-home-filename)))
  "Return t if the filename of FILE-PATH is in the list STRINGS (defaults to 'index' and ox-yaow-filename), or if the filename (without its file extension) is equal to the directory name, else return nil."
  (let ((base (f-base file-path)))
    (or (let ((-compare-fn #'string=)) (-contains? strings base))
	(string= (cadr (reverse (f-split file-path))) base))))

(defun ox-yaow--get-default-indexing-file (path)
  "Return PATH (a directory path) concatenated with 'filename.org' where filename is the name of the directory pointed to by PATH."
  (f-join path (f-swap-ext (f-base path) "org")))

(defun ox-yaow--html-link-stitching-fn (orig-file prev-file next-file up-file)
  "Return html with links to PREV-FILE, NEXT-FILE and UP-FILE if they are non-nil, where the links are relative to ORIG-FILE."
    (let ((attributes (when (or prev-file next-file) "style='float: right;'")))
      (concat (ox-yaow--get-nav-links prev-file next-file)
	      (when up-file (concat "<span " (when attributes attributes) ">Up: "))
	      (ox-yaow--get-html-relative-link
	       up-file
	       (capitalize (s-replace "-" " " (f-base up-file)))
	       :reference-path orig-file)
	      (when up-file "</span>")
	      "<hr>")))

(cl-defun ox-yaow--get-file-ordering-from-index-tree
    (tree &key (headline-fn ox-yaow-headlline-point-to-file-p))
  "Get the ordering of files without extensions suggested by headlines collected from TREE, use HEADLINE-FN to determine whether a given headline should be considered as a file."
  (let ((snd-level-headlines (org-element-map tree 'headline
			       (lambda (hl)
				 (and (funcall headline-fn hl) hl)))))
    ;; Get raw headline and convert to "emacs case"
    (--map (downcase (s-replace " " "-" it))
	   ;; Bit of a hack to remove links
	   (--map (replace-regexp-in-string
		   ".*\\[\\[.+\\]\\[\\(.+\\)\\]\\].*" "\\1"
		   (org-element-property :raw-value it))
		  snd-level-headlines))))

(defun ox-yaow--get-file-ordering-from-index (indexing-file-path)
  "Get the ordering of files in described by the file pointed to by INDEXING-FILE-PATH."
  (with-temp-buffer
    (insert-file-contents indexing-file-path)
    (--map
     ;; `f-swap-ext' will add the extension if none exists
     (if (f-directory? (f-expand it (f-dirname indexing-file-path)))
         it
       (f-swap-ext it "org"))
     (ox-yaow--get-file-ordering-from-index-tree (org-element-parse-buffer)))))

(cl-defun ox-yaow--get-html-relative-link
    (file-path link-text &key (reference-path file-path))
  "Return a HTML link element with text LINK-TEXT and href attribute equal to the path of FILE-PATH relative to REFERENCE-PATH, but with a .html extension."
  (let* ((relative-path (f-relative file-path (f-dirname reference-path)))
	 (html-path (concat (f-no-ext relative-path) ".html")))
    (concat "<a href='"
	    (when (not (char-equal ?. (elt html-path 0))) "./")
	    html-path "'>" link-text "</a>")))

(cl-defun ox-yaow--get-adjacent-strings (target-string strings &key (sort t))
  "Return a plist with property :preceding as the string preceding TARGET-STRING in STRINGS, and :succeeding as the string succeeding TARGET-STRING in STRINGS.  nil is used in either case if no such string exists.  If SORT is t, then sort STRINGS first.  nil is used in place of strings if such a string does not exist.  nil is returned if TARGET-STRING is not in STRINGS."
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
   (when prev-file
     (concat "Previous: " (ox-yaow--get-html-relative-link
			   prev-file
			   (capitalize (s-replace "-" " " (f-base prev-file))))))
   (when (and prev-file next-file) ", ")
   (when next-file
     (concat "Next: " (ox-yaow--get-html-relative-link
		       next-file
		       (capitalize (s-replace "-" " " (f-base next-file))))))))

(cl-defun ox-yaow--get-up-file
    (target-file-path &key (indexing-file-p ox-yaow-indexing-file-p)
		      (get-default-indexing-file ox-yaow-get-default-indexing-file))
  "If TARGET-FILE-PATH points to an indexing file (determined by INDEXING-FILE-P), return the path to the indexing file in the parent directory of TARGET-FILE-PATH (or return the result of GET-DEFAULT-INDEXING-FILE if no such file exists).  Else return the file path of the indexing file in the same directory as TARGET-FILE-PATH."
  (let* ((directory (if (funcall indexing-file-p target-file-path)
		       (f-parent (f-parent target-file-path))
		     (f-parent target-file-path)))
	 (indexing-file (car (-filter indexing-file-p (f-files directory)))))
    (if indexing-file indexing-file (funcall get-default-indexing-file directory))))

(defun ox-yaow--get-file-ordering-from-directory
    (directory-path &optional show-indexing-files)
  "Get the ordering of files in DIRECTORY-PATH, when SHOW-INDEXING-FILES is non-nil, also include indexing files."
  (let ((indexing-file
         (funcall ox-yaow-get-default-indexing-file directory-path)))
    (if (f-exists-p indexing-file)
        (--map (f-join directory-path it)
               (ox-yaow--get-file-ordering-from-index indexing-file))
      (ox-yaow--org-entries directory-path (not show-indexing-files)))))

(cl-defun ox-yaow--get-adjacent-files
    (target-file file-list &key (indexing-file-p ox-yaow-indexing-file-p))
  "Get files before and after TARGET-FILE in FILE-LIST.  These should all be full file paths.  INDEXING-FILE-P is a predicate determining whether a given file should be treated as an indexing file."
  (let* ((indexing-file (car (-filter indexing-file-p file-list)))
	 (base-files (--map (f-base it) file-list))
	 (file-ordering
	  (if indexing-file
	      ;; Ensure links found in indexing file point to files in same dir
	      (--filter (member (f-base it) base-files)
			(--map (f-base it)
			       (ox-yaow--get-file-ordering-from-index indexing-file)))
	    base-files)))
    (ox-yaow--get-adjacent-strings (f-base target-file)
				   file-ordering
                                   ;; TODO sort outside the function
				   :sort (unless indexing-file))))

(defun ox-yaow-template (contents info)
  "Transcode CONTENTS into yaow html format.  INFO is a plist used as a communication channel."
  (let* ((file-path (plist-get info :input-file))
	 (filename (f-base file-path))
	 (directory (f-dirname filename))
	 (org-paths-same-level (f-files directory (lambda (file) (s-suffix? "org" file))))
	 (adj-files (ox-yaow--get-adjacent-files filename org-paths-same-level))
	 (next-file (plist-get adj-files :succeeding))
	 (prev-file (plist-get adj-files :preceding))
	 (up-file (ox-yaow--get-up-file file-path))
	 (base-html (org-html-template contents info)))
    (if (string= filename ox-yaow-wiki-home-filename) base-html
      (replace-regexp-in-string "<h1" (concat (funcall ox-yaow-html-link-stitching-fn
						       file-path
						       prev-file next-file up-file)
					      "<h1")
				base-html))))

(cl-defun ox-yaow--get-index-file-str (file-path file-path-list &key (depth 2))
  "Return the contents of the indexing file FILE-PATH as a string, containing links to files in FILE-PATH-LIST, recursing down DEPTH directories."
  (cl-labels
      ((to-title (file-path) (capitalize (s-replace "-" " " (f-base file-path))))
       (index-file-str-rec
        (file-path file-path-list depth base-path)
        (mapconcat
	 (lambda (path)
	   (concat
	    (format "** [[./%s][%s]]\n"
		    (f-swap-ext
                     (f-relative
                      (if (f-directory? path)
			  (funcall ox-yaow-get-default-indexing-file path)
			path)
                      (f-dirname base-path))
                     "html")
                    (to-title path))
	    (when-let (((and (< 0 depth) (f-directory? path)))
                       (lower-str
		        (index-file-str-rec
                         path
                         (ox-yaow--get-file-ordering-from-directory path)
			 (1- depth)
                         base-path)))
              (s-replace "* " "** " lower-str))))
	 file-path-list "")))
    (concat
     "#+TITLE: " (to-title file-path) "\n"
     (index-file-str-rec file-path file-path-list depth file-path))))

(defun ox-yaow--prep-directory (directory-path &optional depth no-log force)
  "Create an indexing file at DIRECTORY-PATH if appropriate.
Check if the (full) path described by DIRECTORY-PATH has an indexing file,

if it does not, create one.
If FORCE is non-nil then overwrite the existing indexing file.
The created indexing file will show subdirectories up to DEPTH.
If NO-LOG is non-nil then this file will not be removed."
  (let* ((files (f-files directory-path (lambda (f) (s-ends-with-p ".org" f))))
	 (indexing-file (-first ox-yaow-indexing-file-p files)))
    (when (or force (not indexing-file))
      ;; Create file
      (let ((indexing-file-name
	      (funcall ox-yaow-get-default-indexing-file directory-path)))
	(with-temp-buffer
	  (insert (ox-yaow--get-index-file-str
		   indexing-file-name
		   (ox-yaow--org-entries directory-path t)
		   :depth (or depth 1)))
	  (write-region (point-min) (point-max) indexing-file-name)
          (unless no-log
            (setq ox-yaow--generated-files (cons indexing-file-name
                                                 ox-yaow--generated-files))))))))

(defun ox-yaow-preparation-fn (project-alist)
  "Create temporary indexing files for the project described in PROJECT-ALIST."
  (let ((src-dir (plist-get project-alist :base-directory))
	(depth (plist-get project-alist :ox-yaow-depth)))
    (cl-loop for directory in (-filter #'ox-yaow--org-assoc-file-p
				       (f-directories src-dir nil t))
	     do
	     (ox-yaow--prep-directory directory depth))))

(defun ox-yaow-completion-fn (_)
  "Remove temporary indexing files for the project described in PROJECT-ALIST."
  (cl-loop for generated-file in ox-yaow--generated-files do
	   ;; Just to be extra careful
	   (when (f-exists-p generated-file) (f-delete generated-file))
	   (message (concat "Deleted generated file: " generated-file)))
  (setq ox-yaow--generated-files nil))


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
  (org-publish-org-to 'ox-yaow-html filename ".html" plist pub-dir))


;; Interactive functions

;;;###autoload
(defun ox-yaow-generate-indexing-file (depth)
  "Generate an indexing with for the directory which the current file resides.
The indexing file will have depth DEPTH."
  (interactive "nPlease input the indexing file depth: ")
  (if buffer-file-name
      (let* ((parent-dir (f-parent (buffer-file-name)))
             (indexing-file (funcall ox-yaow-get-default-indexing-file parent-dir)))
        (ox-yaow--prep-directory parent-dir depth t t)
        (find-file-other-window indexing-file))
    (message "Not visiting a file!")))

(provide 'ox-yaow)

;;; ox-yaow.el ends here
