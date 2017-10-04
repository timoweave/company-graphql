;;; company-graphql.el --- completion for graphql file                     -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Timothy Shiu

;; Author: Timothy Shiu <timoweave@gmail.com>
;; Keywords: company graphql completion
;; Package-Requires: ((emacs "25.2.1") (company "0.9.4"))

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

;; Read README for more details.

;;; Code:

(require 'json)
(require 'subr-x)
(require 'company)

(defgroup company-graphql nil
  "Completion backends for commit templates."
  :tag "company-graphql"
  :group 'company)

(defcustom company-graphql-schema-filename "./schema.json"
  "Filename that has graphql schema."
  :type 'file
  :group 'company-graphql)

(defcustom company-graphql-schema-server nil
  "Server endpoint that has graphql __schema."
  :type 'string
  :group 'company-graphql)

(defconst company-graphql-schema-root ""
  "Schema Root.")

(defun company-graphql--candidates-parent (items parent)
  "Get hashtables."
  (cl-remove-if-not
   (lambda (item) (string= parent (gethash "name" item)))
   items))

(defun company-graphql--candidates-children-type (item)
  "Get the type without container [] nor constrianted !."
  (or (and (gethash "type" field)  (gethash "name" (gethash "type" field)))
      (and (gethash "type" field)  (gethash "name" (gethash "ofType" (gethash "type" field))))
      nil))

(defun company-graphql--candidates-children (items)
  "List of Fields."
  (let ((fields '())
	(names '()))
    (dolist (item items)
      (push (gethash "fields" item) fields))
    (dolist (field (car fields))
      (let* ((name (gethash "name" field))
	     (annotation (company-graphql--candidates-children-type field))
	     (meta "TBD")
	     )
	(push (cons (propertize name :annotation annotation :meta meta) annotation) names)))
    names))

(defun company-graphql--candidates-visit-dfs (items)
  "Get List of Fields."
  (dolist (item items)
    (let ((name (gethash "name" item))
	  (fields (gethash "fields" item))
	  (keys '()))
      (if (not (listp fields))
	  (setq fields "")
	(mapcar
	 (lambda (field)
	   (and field
		(push (cons
		       (gethash "name" field)
		       (company-graphql--candidates-children-type field)) keys)))
	 fields)
	(setq fields keys))
      (message "parent = %s children = %s" name fields)
      fields)))

(defun company-graphql--candidates-add-subroot (operation-type type name)
  "Make operation into type."
  (let* ((operation-type (gethash type schema))
	 (operation (make-hash-table :test 'equal)))
    (and operation-type
	 (progn
	   (puthash "kind" "Object" operation-type)
	   (puthash "ofType" nil operation-type)
	   (puthash "name" name operation)
	   (puthash "fields" nil operation)
	   (puthash "type" operation-type operation)
	   operation))))

(defun company-graphql--candidates-add-root (schema)
  "Make operation into type."
  (let ((operations '())
	(query (company-graphql--candidates-add-subroot
		schema "queryType" "query"))
	(mutation (company-graphql--candidates-add-subroot
		   schema "mutationType" "mutation"))
	(subscription (company-graphql--candidates-add-subroot
		       schema "subscriptionType" "subscription")))
    (let ((json-object-type 'hash-table)
	  (json-array-type 'list)
	  (json-key-type 'string)
	  (root (make-hash-table :test 'equal)))
      (when query (push query operations))
      (when mutation (push mutation operations))
      (when subscription (push subscription operations))
      (puthash "name" company-graphql-schema-root root)
      (puthash "fields" operations root)
      root)))

(setq company-graphql--candidates-items-cache nil)
(defun company-graphql--candidates-items ()
  "Parse GraphQL data.__schema.types JSON into hashtable"
  (or company-graphql--candidates-items-cache
      (set (make-local-variable 'company-graphql--candidates-items-cache)
	   (let* ((json-object-type 'hash-table)
		  (json-array-type 'list)
		  (json-key-type 'string)
		  (json (json-read-file company-graphql-schema-filename)))
	     (let* ((data (gethash "data" json))
		    (schema (gethash "__schema" data))
		    (types (gethash "types" schema)))
	       (push (company-graphql--candidates-add-root schema) types)
	       types)))))

(defun company-graphql--beginning-of-query ()
  "Move the point to the beginning of the current query."
  (interactive)
  (while (and (> (point) (point-min))
              (or (> (current-indentation) 0)
                  (> (car (syntax-ppss)) 0)))
    (forward-line -1)))

(defun company-graphql--path-names-root ()
  "Path root, query, mutation, subscription, where anynomous is query"
  (interactive)
  (let ((op company-graphql-schema-root)
	(name nil)
	(begin (point))
	(end (point))
	(op-name "\\(query\\|mutation\\|subscription\\)[ \t\n\r]*\\([^ \t\n\r\(\{]+\\)?")
	(sub-string nil))
    (save-excursion
      (condition-case nil
	  (progn
	    (search-backward "}")
	    (forward-char)
	    (setq begin (point)))
	(error (progn (setq begin (point-min)))))
      (condition-case nil
	  (progn
	    (setq sub-string (buffer-substring-no-properties begin end))
	    (when (string-match op-name sub-string)
	      (setq op (match-string 1 sub-string))
	      (setq name (match-string 2 sub-string))))
	(error nil))
      )
    (list op name)))

(defun company-graphql--path-names ()
  "path names of query"
  (interactive)
  (save-excursion
    (let ((type-name "\\([^ \t\n\r\(\{]+\\)[ \t\n\r]*\\(([^\(]+)\\)?[ \t\n\r]*{$")
	  (sub-string nil)
	  (path '())
	  (last-points '())
	  (last-substrings '()))    
      (condition-case nil
	  (while t
	    (backward-up-list)
	    (push (point) last-points))
	(error (push (car (company-graphql--path-names-root)) last-substrings)))
      (while (> (cl-list-length last-points) 1)
	(setq sub-string (buffer-substring-no-properties (1+ (nth 0 last-points)) (1+ (nth 1 last-points))))
	(setq sub-string (replace-regexp-in-string "\n" "" sub-string))
	(when (string-match type-name sub-string)
	  (push (match-string 1 sub-string) last-substrings))
	(pop last-points))
      (setq last-substrings (reverse last-substrings))
      last-substrings)))

(defun company-graphql--path-names-zipper (name lookup)
  "Zip given name with its GraphQL type."
  (let* ((type (cdr (assoc name lookup)))
	 (ans (cons name type)))
    (setq lookup (company-graphql--candidates-children
		  (company-graphql--candidates-parent
		   (company-graphql--candidates-items) type)))
    (cons ans lookup)))

(defun company-graphql--path-names-types (path-names)
  "Build name and type pair for a given path."
  (let ((table (company-graphql--candidates-children
		(company-graphql--candidates-parent
		 (company-graphql--candidates-items) "")))
	(name-types '()))
    (mapcar
     (lambda(name)
       (let ((next (company-graphql--path-names-zipper name table)))
	 (setq table (cdr next))
	 (car next)))
     path-names)))

(defun company-graphql--prefix ()
  "Company-GraphQL Prefix."
  (company-grab-symbol-cons "\\.\\|->" 1))

(defun company-graphql--annotation (candidate)
  "Company-GraphQL Annotation."
  (format " %s" (get-text-property 0 :annotation candidate)))

(defun company-graphql--meta (candidate)
  "Company-GraphQL Meta."
  ;; TBD
  (format "" (get-text-property 0 :meta candidate)))

(defun company-graphql--candidates (prefix)
  "Company-GraphQL Candidates"
  (let* ((text (substring-no-properties prefix))
	 (path (company-graphql--path-names))
	 (scope (if (string= company-graphql-schema-root (car path))
		    company-graphql-schema-root (cdr (car (reverse (company-graphql--path-names-types path))))))
	 (children (company-graphql--candidates-children
		    (company-graphql--candidates-parent
		     (company-graphql--candidates-items) scope)))
	 (filtered (cl-remove-if-not
		    (lambda (item) (string-prefix-p text (car item)))
		    children))
	 (answer (cl-mapcar (lambda (x) (car x)) filtered)))
    answer))

(defun company-graphql (command &optional arg &rest ignored)
  "Company-GraphQL entry command."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-graphql))
    (prefix (company-graphql--prefix))
    (candidates (company-graphql--candidates arg))
    (annotation (company-graphql--annotation arg))
    (meta (company-graphql--meta arg))
    (ignore-case t)
    (no-cache t)))

(provide 'company-graphql)
;;; company-graphql.el ends here
