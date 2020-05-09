;;; test-yaow.el --- Tests for yaow.el -*- lexical-binding: t -*-


;;; Commentary:

;; Tests for yaow.el

;;; Code:

(require 'buttercup)
(require 'yaow)

(describe "The yaow package"

	  (describe "yaow--get-adjacent-files"
		    :var ((files-in-dir '("a.org" "b.org" "hi.org" "z.org")))

		    (it "should return correct adjacent files for middle file"
			(expect (yaow--get-adjacent-files "b.org" files-in-dir)
				:to-equal
				'("a.org" . "hi.org")))

		    (it "should return sole adjacent file for beginning file"
			(expect (yaow--get-adjacent-files "a.org" files-in-dir)
				:to-equal
				'(nil . "b.org")))

		    (it "should return sole adjacent file for end file"
			(expect (yaow--get-adjacent-files "z.org" files-in-dir)
				:to-equal
				'("hi.org" . nil)))

		    (it "should return no adjacent files for one file in list"
			(expect (yaow--get-adjacent-files "z.org" '("z.org"))
				:to-equal
				'(nil . nil)))

		    (it "should return correct adjacent files for no sort"
			(let ((files-in-dir-no-sort
			       '("z.org" "b.org" "hi.org" "a.org")))
			  (expect (yaow--get-adjacent-files "b.org"
							    files-in-dir-no-sort
							    :sort nil)
				  :to-equal
				  '("z.org" . "hi.org")))))

	  (describe "yaow--get-file-ordering-from-index-tree"
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
			(expect (yaow--get-file-ordering-from-index-tree
				 tree1 :headline-fun fn)
				:to-equal
				'("ideals.org" "factor-groups.org" "domains.org")))

		    (it "should return headlines in order for linked index file"
			(expect (yaow--get-file-ordering-from-index-tree
				 tree1 :headline-fun fn)
				:to-equal
				'("ideals.org" "factor-groups.org" "domains.org")))
		    
		    ))
