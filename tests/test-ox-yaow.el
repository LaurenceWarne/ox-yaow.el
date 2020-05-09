;;; test-ox-yaow.el --- Tests for ox-yaow.el -*- lexical-binding: t -*-


;;; Commentary:

;; Tests for ox-yaow.el

;;; Code:

(require 'buttercup)
(require 'ox-yaow)

(describe "The ox-yaow package"

	  (describe "ox-yaow--get-adjacent-files"
		    :var ((files-in-dir '("a.org" "b.org" "hi.org" "z.org")))

		    (it "should return correct adjacent files for middle file"
			(expect (ox-yaow--get-adjacent-files "b.org" files-in-dir)
				:to-equal
				'(:preceding-file "a.org" :succeeding-file "hi.org")))

		    (it "should return sole adjacent file for beginning file"
			(expect (ox-yaow--get-adjacent-files "a.org" files-in-dir)
				:to-equal
				'(:preceding-file nil :succeeding-file "b.org")))

		    (it "should return sole adjacent file for end file"
			(expect (ox-yaow--get-adjacent-files "z.org" files-in-dir)
				:to-equal
				'(:preceding-file "hi.org" :succeeding-file nil)))

		    (it "should return no adjacent files for one file in list"
			(expect (ox-yaow--get-adjacent-files "z.org" '("z.org"))
				:to-equal
				'(:preceding-file nil :succeeding-file nil)))

		    (it "should return correct adjacent files for no sort"
			(let ((files-in-dir-no-sort
			       '("z.org" "b.org" "hi.org" "a.org")))
			  (expect (ox-yaow--get-adjacent-files "b.org"
							    files-in-dir-no-sort
							    :sort nil)
				  :to-equal
				  '(:preceding-file "z.org" :succeeding-file "hi.org"))))

		    (it "should return nil when file no in file list"
			(expect (ox-yaow--get-adjacent-files "not-in.org" files-in-dir)
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
		    
		    ))
