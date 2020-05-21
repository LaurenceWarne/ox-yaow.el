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
				nil))

		    )

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
				  (org-element-parse-buffer)))
			  (fn (lambda (hl) (= 2 (org-element-property :level hl)))))
		    
		    (it "should return headlines in order for non-linked index file"
			(expect (ox-yaow--get-file-ordering-from-index-tree
				 tree1 :headline-fun fn)
				:to-equal
				'("ideals.org" "factor-groups.org" "domains.org")))

		    (it "should return headlines in order for linked index file"
			(expect (ox-yaow--get-file-ordering-from-index-tree
				 tree1 :headline-fun fn)
				:to-equal
				'("ideals.org" "factor-groups.org" "domains.org")))
		    
		    )

	  (describe "ox-yaow-get-default-indexing-file"

		    (it "should return correct indexing file with string ending with slash"
			(expect (funcall ox-yaow-get-default-indexing-file "/my/dir/")
				:to-equal
				"/my/dir/dir.org"))

		    (it "should return correct indexing file with string ending without slash"
			(expect (funcall ox-yaow-get-default-indexing-file "/my/dir/")
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

		    (it "should return correct string for files"
			(expect (ox-yaow--get-index-file-str
				 "/home/maths/maths.org"
				 '("/home/maths/questions.org"
				   "/home/maths/number/numbers.org"
				   "/home/maths/other-thing.org"))
				:to-equal
				"#+TITLE: Maths
* Maths
** [[./questions.html][Questions]]
** [[./number/numbers.html][Numbers]]
** [[./other-thing.html][Other Thing]]
"
))))
