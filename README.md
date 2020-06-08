# Yet Another Org Wiki

ox-yaow - yet another org wiki, is a lightweight as-is org to html export option. It doesn't require any modifications to existing org files, and instead uses the directory structure of the source files to generate links.

It's main use case is static site generation, perfect for publishing notes, etc to Jekyll. You can see an example [here](https://laurencewarne.github.io/wiki/wiki.html), which were generated from the org files located [here](https://github.com/LaurenceWarne/org-files).

## Usage

In order to use the package we will need to add a new project to ```org-publish-project-alist``` or edit an existing one:

```lisp
(setq org-publish-project-alist
	  '(("wiki"
		  ;;-------------------------------
		  ;; Standard org publish options
		  ;;-------------------------------
		  :base-directory "~/org/"
		  :base-extension "org"
		  :publishing-directory "~/wiki/"
		  ;; We use css from read-the-org
		  :html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"https://fniessen.github.io/org-html-themes/styles/readtheorg/css/htmlize.css\"/><link rel=\"stylesheet\" type=\"text/css\" href=\"https://fniessen.github.io/org-html-themes/styles/readtheorg/css/readtheorg.css\"/><script src=\"https://ajax.googleapis.com/ajax/libs/jquery/2.1.3/jquery.min.js\"></script><script src=\"https://maxcdn.bootstrapcdn.com/bootstrap/3.3.4/js/bootstrap.min.js\"></script><script type=\"text/javascript\" src=\"https://fniessen.github.io/org-html-themes/styles/lib/js/jquery.stickytableheaders.min.js\"></script><script type=\"text/javascript\" src=\"https://fniessen.github.io/org-html-themes/styles/readtheorg/js/readtheorg.js\"></script>"
		  :html-preamble t
		  :recursive t
		  :publishing-function ox-yaow-publish-to-html
		  ;; Auto generates indexing files
		  :preparation-function ox-yaow-preparation-fn
		  ;; Removes auto-generated files
		  :completion-function ox-yaow-completion-fn
		  ;;------------------------------
		  ;; Options specific to ox-yaow
		  ;;------------------------------
		  :ox-yaow-depth 1)))
```

We can then ```C-c C-e``` (```org-export-dispatch```) from within any org mode file and select ```P``` to export our project.

## Installation

## Similar Packages

### [org-brain](https://github.com/Kungsgeten/org-brain)

Also see [org-brain-export](https://github.com/Kungsgeten/org-brain-export).

### [org-wiki](https://github.com/caiorss/org-wiki)

### [org2jekyll](https://github.com/ardumont/org2jekyll)
