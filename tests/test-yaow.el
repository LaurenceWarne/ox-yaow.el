;;; test-yaow.el --- Tests for yaow.el -*- lexical-binding: t -*-


;;; Commentary:

;; Tests for yaow.el

;;; Code:

(require 'buttercup)
(require 'yaow)

(describe "The yaow package"
	  (describe " yaow--get-adjacent-files"
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
			     '("hi.org" . nil)))))
