(require 's)
(require 'buttercup)
(load-file "company-graphql.el")

(describe
 "path names"
 (it "find '(\"query\")"
   (expect (test-path-names "query")
	   :to-have-same-items-as
	   '("query")))
 (it "find '(\"query\" \"onePost\")"
   (expect (test-path-names "query hello { onePost {")
	   :to-have-same-items-as
	   '("query" "onePost")))
 (it "find '(\"query\" \"onePost\" \"tags\")"
   (expect (test-path-names "query hello { onePost { tags {")
	   :to-have-same-items-as
	   '("query" "onePost" "tags")))
 )

(describe
 "path names with spaces"
 (it "find '(\"query\")"
   (expect (test-path-names-with-spaces "query")
	   :to-have-same-items-as
	   '("query")))
 (it "find '(\"query\" \"onePost\")"
   (expect (test-path-names-with-spaces "query hello { onePost {")
	   :to-have-same-items-as
	   '("query" "onePost")))
 (it "find '(\"query\" \"onePost\" \"tags\")"
   (expect (test-path-names-with-spaces "query hello { onePost { tags {")
	   :to-have-same-items-as
	   '("query" "onePost" "tags")))
 )

(describe
 "path names and its type"
 (it "find '(\"query\")"
   (expect (test-path-types '("query"))
	   :to-have-same-items-as
	   '(("query" . "Query"))))
 (it "find '(\"query\" \"onePost\")"
   (expect (test-path-types '("query" "onePost"))
	   :to-have-same-items-as
	   '(("query" . "Query") ("onePost" . "Post"))))
 (it "find '(\"query\" \"onePost\" \"tags\")"
   (expect (test-path-types '("query" "onePost" "tags"))
	   :to-have-same-items-as
	   '(("query" . "Query")  ("onePost" . "Post") ("tags" . "Tag"))))
 )

(describe
 "path names and its type with args"
 (it "find '(\"query\" \"onePost\")"
   (expect (test-path-types (list "query" "onePost" (test-name-arg "onePost")))
	   :to-have-same-items-as
	   (list (cons "query" "Query") (cons "onePost" "Post") (test-name-arg-pair "onePost"))))
 ;; (it "find \"query\" \"onePost\" \"tags\""d
 ;;   (expect (test-path-types '("query" "onePost" "tags"))
 ;; 	   :to-have-same-items-as
 ;; 	   '(("query" . "Query")  ("onePost" . "Post") ("tags" . "Tag") ("tags.args"))))
 )

(defun test-name-arg (name)
  "Return the made-up name.args"
  (format "%s.%s" name company-graphql-schema-args))

(defun test-name-arg-pair (name)
  "Return the made-up (name.args . name.args)"
  (let ((name-arg (test-name-arg name)))
    (cons name-arg name-arg)))

(defun test-path-types (paths &optional schema-filename)
  "Testing company-graphql-path-types"
  (let ((buffer (get-buffer company-graphql-schema-introspect-buffer)))
    (when (null buffer)
      (with-current-buffer (get-buffer-create company-graphql-schema-introspect-buffer)
	(insert-file "./tests/introspection-schema.json")
	(company-graphql-schema-types)
	))
    (company-graphql-path-types paths)))

(defun test-path-names (content &optional buffer-name)
  "Testing company-graphql-path-names"
  (let ((test-buffer (or buffer-name "*tests*")))
    (with-current-buffer (get-buffer-create test-buffer)
      (erase-buffer)
      (insert content)
      (company-graphql-path-names))))

(defun test-path-names-with-spaces (content &optional more-spaces buffer-name)
  "Testing company-graphql-path-names"
  (let ((spaces (or more-spaces " \n \t  \n \t ")))
    (test-path-names (s-replace " " spaces content) buffer-name)))
