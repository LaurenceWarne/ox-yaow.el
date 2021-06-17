;;; test-ox-yaow.el --- Tests for ox-yaow.el -*- lexical-binding: t -*-


;;; Commentary:

;; Tests for ox-yaow.el

;;; Code:

(require 'buttercup)
(require 'ox-yaow)

(describe "The ox-yaow package"

  (describe "ox-yaow--get-adjacent-strings"
    :var ((files-in-dir '("a.org" "b.org" "hi.org" "z.org")))

    (it "should return correct adjacent files for middle file"
      (expect (ox-yaow--get-adjacent-strings "b.org" files-in-dir)
	      :to-equal
	      '(:preceding "a.org" :succeeding "hi.org")))

    (it "should return sole adjacent file for beginning file"
      (expect (ox-yaow--get-adjacent-strings "a.org" files-in-dir)
	      :to-equal
	      '(:preceding nil :succeeding "b.org")))

    (it "should return sole adjacent file for end file"
      (expect (ox-yaow--get-adjacent-strings "z.org" files-in-dir)
	      :to-equal
	      '(:preceding "hi.org" :succeeding nil)))

    (it "should return no adjacent files for one file in list"
      (expect (ox-yaow--get-adjacent-strings "z.org" '("z.org"))
	      :to-equal
	      '(:preceding nil :succeeding nil)))

    (it "should return correct adjacent files for no sort"
      (let ((files-in-dir-no-sort
	     '("z.org" "b.org" "hi.org" "a.org")))
	(expect (ox-yaow--get-adjacent-strings "b.org"
					       files-in-dir-no-sort
					       :sort nil)
		:to-equal
		'(:preceding "z.org" :succeeding "hi.org"))))

    (it "should return nil when file no in file list"
      (expect (ox-yaow--get-adjacent-strings "not-in.org" files-in-dir)
	      :to-equal
	      nil)))

  (describe "ox-yaow--get-file-ordering-from-index-tree"
    :var ((tree1 (with-temp-buffer
		   (insert "#+TITLE: Algebra
* Algebra
** [[./ideals.org][Ideals]]
** [[./factor-groups.org][Factor Groups]]
** [[./domains.org][Domains]]")
		   (org-element-parse-buffer)))
	  (tree2 (with-temp-buffer
		   (insert "#+TITLE: Algebra
* Algebra
** Ideals
** Factor Groups
** Domains")
		   (org-element-parse-buffer))))
    
    (it "should return headlines in order for non-linked index file"
      (expect (ox-yaow--get-file-ordering-from-index-tree tree1)
	      :to-equal
	      '("ideals" "factor-groups" "domains")))

    (it "should return headlines in order for linked index file"
      (expect (ox-yaow--get-file-ordering-from-index-tree tree1)
	      :to-equal
	      '("ideals" "factor-groups" "domains"))))

  (describe "ox-yaow--indexing-file-p"
    :var ((strings '("hi")))
    (it "should return true on file name in strings"
      (expect (ox-yaow--indexing-file-p
	       "/home/org/maths/number/hi.org"
	       :strings strings)
	      :to-equal
	      t))

    (it "should return nil on file name not in strings and not named same as directory"
      (expect (ox-yaow--indexing-file-p
	       "/home/org/maths/number/normal-file.org"
	       :strings strings)
	      :to-equal
	      nil))

    (it "should return true on file name same as directory"
      (expect (ox-yaow--indexing-file-p
	       "/home/org/maths/number/number.org"
	       :strings strings)
	      :to-equal
	      t)))

  (describe "ox-yaow--get-default-indexing-file"

    (it "should return correct indexing file with string ending with slash"
      (expect (ox-yaow--get-default-indexing-file "/my/dir/")
	      :to-equal
	      "/my/dir/dir.org"))

    (it "should return correct indexing file with string ending without slash"
      (expect (ox-yaow--get-default-indexing-file "/my/dir/")
	      :to-equal
	      "/my/dir/dir.org")))

  (describe "ox-yaow--get-html-relative-link"

    (it "should return correct relative link for file same directory"
      (expect (ox-yaow--get-html-relative-link
	       "/home/maths/maths.org"
	       "text"
	       :reference-path "/home/maths/number.org")
	      :to-equal
	      "<a href='./maths.html'>text</a>"))

    (it "should return correct relative link for file outer directory"
      (expect (ox-yaow--get-html-relative-link
	       "/home/base.org"
	       "text"
	       :reference-path "/home/maths/number.org")
	      :to-equal
	      "<a href='../base.html'>text</a>"))

    (it "should return correct relative link for file inner directory"
      (expect (ox-yaow--get-html-relative-link
	       "/home/maths/algebra/sub.org"
	       "text"
	       :reference-path "/home/maths/number.org")
	      :to-equal
	      "<a href='./algebra/sub.html'>text</a>"))
    
    (it "should return correct relative link for no ref file"
      (expect (ox-yaow--get-html-relative-link
	       "/home/maths/algebra/sub.org"
	       "text")
	      :to-equal
	      "<a href='./sub.html'>text</a>")))

  (describe "ox-yaow--get-index-file-str"
    (cl-defun index-file-str-stubbed-env
        (file-path
         file-path-list
         &optional
         file-blacklist
         &key (depth 2)
         (propagation-fn #'ignore)
         (propagate-p #'ignore))
      (cl-letf (((symbol-function 'ox-yaow--get-file-ordering-from-directory)
                 propagation-fn)
                ((symbol-function 'f-directory?) propagate-p))
        (ox-yaow--get-index-file-str
         file-path file-path-list file-blacklist :depth depth)))

    (it "should return correct string for files"
      (expect (index-file-str-stubbed-env
	       "/home/maths/maths.org"
	       '("/home/maths/questions.org"
		 "/home/maths/number/numbers.org"
		 "/home/maths/other-thing.org")
               nil
	       :depth 0)
	      :to-equal
	      "#+TITLE: Maths
** [[./questions.html][Questions]]
** [[./number/numbers.html][Numbers]]
** [[./other-thing.html][Other Thing]]
"))
    
    (it "should return sub-levels correctly"
      (cl-flet ((custom-propagation-fn
		 (path _ _)
		 (cond ((string= path "/maths/algebra")
			'("/maths/algebra/groups.org"
			  "/maths/algebra/symbols.org"))
		       ((string= path "/maths/numbers")
			'("/maths/algebra/one.org"
			  "/maths/algebra/two.org"))))
		(custom-propagate-p
		 (path)
		 (or (string= path "/maths/algebra")
		     (string= path "/maths/numbers"))))
	(expect (index-file-str-stubbed-env
		 "/maths/algebra"
		 '("/maths/algebra"
		   "/maths/questions.org"
		   "/maths/numbers"
		   "/maths/other-thing.org")
                 nil
		 :depth 1
		 :propagation-fn #'custom-propagation-fn
		 :propagate-p #'custom-propagate-p)
		:to-equal
		"#+TITLE: Algebra
** [[./algebra/algebra.html][Algebra]]
*** [[./algebra/groups.html][Groups]]
*** [[./algebra/symbols.html][Symbols]]
** [[./questions.html][Questions]]
** [[./numbers/numbers.html][Numbers]]
*** [[./algebra/one.html][One]]
*** [[./algebra/two.html][Two]]
** [[./other-thing.html][Other Thing]]
")))))
