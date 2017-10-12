(require 's)
(require 'buttercup)

(load-file "company-graphql.el")

(describe
 "path names"
 (it "find \"query\""
   (expect (test-path-names "query")
	   :to-have-same-items-as
	   '("query")))
 (it "find \"query\" \"onePost\""
   (expect (test-path-names "query hello { onePost {")
	   :to-have-same-items-as
	   '("query" "onePost")))
 (it "find \"query\" \"onePost\" \"tags\""
   (expect (test-path-names "query hello { onePost { tags {")
	   :to-have-same-items-as
	   '("query" "onePost" "tags")))
 )

(describe
 "path names with spaces"
 (it "find \"query\""
   (expect (test-path-names-with-spaces "query")
	   :to-have-same-items-as
	   '("query")))
 (it "find \"query\" \"onePost\""
   (expect (test-path-names-with-spaces "query hello { onePost {")
	   :to-have-same-items-as
	   '("query" "onePost")))
 (it "find \"query\" \"onePost\" \"tags\""
   (expect (test-path-names-with-spaces "query hello { onePost { tags {")
	   :to-have-same-items-as
	   '("query" "onePost" "tags")))
 )

(describe
 "path types"
 (it "find \"query\""
   (expect (test-path-types '("query"))
	   :to-have-same-items-as
	   '(("query" . "Query"))))
 (it "find \"query\" \"onePost\""
   (expect (test-path-types '("query" "onePost"))
	   :to-have-same-items-as
	   '(("query" . "Query") ("onePost" . "Post"))))
 (it "find \"query\" \"onePost\" \"tags\""
   (expect (test-path-types '("query" "onePost" "tags"))
	   :to-have-same-items-as
	   '(("query" . "Query")  ("onePost" . "Post") ("tags" . "Tag"))))
 )

(defun test-path-types (paths &optional schema-filename)
  "Testing company-graphql--path-types"
  (let ((company-graphql-schema-filename (or schema-filename "./schema.json")))
    (company-graphql--path-types paths)))

;;(test-path-types "query")

(defun test-path-names (content &optional buffer-name)
  "Testing company-graphql--path-names"
  (let ((test-buffer (or buffer-name "*tests*")))
    (with-current-buffer (get-buffer-create test-buffer)
      (erase-buffer)
      (insert content)
      (company-graphql--path-names))))

(defun test-path-names-with-spaces (content &optional more-spaces buffer-name)
  "Testing company-graphql--path-names"
  (let ((spaces (or more-spaces " \n \t  \n \t ")))
    (test-path-names (s-replace " " spaces content) buffer-name)))
