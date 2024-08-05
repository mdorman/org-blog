;;; org-blog-wp.el --- Interact with WordPress
;;; -*- lexical-binding: t -*-

;; Copyright (C) 2013 Michael Alan Dorman

;; Author: Michael Alan Dorman <mdorman at ironicdesign dot com>
;; Keywords: blog, org-mode

;; This file is part of org-blog.

;; org-blog is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; org-blog is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; GNU General Public License at <http://www.gnu.org/licenses/>.

;;; Code:

(provide 'org-blog-wp)

(require 'org)
(require 'org-blog)
(require 'xml-rpc)

(add-to-list 'org-blog-engine-alist '("wp"))

(eval-when-compile
  (require 'cl-lib))

;;;; Transformation to and from a post

(defconst org-blog-wp-alist
  (list (cons :category "category")
        (cons :content "post_content")
        (cons :date "post_date_gmt")
        (cons :description "post_excerpt")
        (cons :id "post_id")
        (cons :keywords "post_tag")
        (cons :link "link")
        (cons :name "post_name")
        (cons :parent "post_parent")
        (cons :status "post_status")
        (cons :title "post_title")
        (cons :type "post_type")))

(defun org-blog-post-to-wp (post)
  "Transform a post into a structure for submitting to WordPress.

This is largely about mapping tag names, though the handling of
`category' and `tags' is little more complex as the WordPress API
now groups them as `taxonomies', and requires a hierarchical
structure to differentiate them.

For convenience in testing and inspection, the resulting alist is
sorted."
  (sort
   (cl-reduce
    (lambda (wp new)
      (let ((k (car new))
            (v (cdr new)))
        (cond ((eq v nil)
               wp)
              ((eq :category k)
               (org-blog-post-to-wp-add-taxonomy wp "category" v))
              ((eq :date k)
               ;; Convert to GMT by adding seconds offset
               (let ((gmt (time-add v (seconds-to-time (- (car (current-time-zone)))))))
                 (cons (cons "post_date_gmt" (list :datetime gmt)) wp)))
              ((eq :keywords k)
               (org-blog-post-to-wp-add-taxonomy wp "post_tag" v))
              ((eq :title k)
               (cons (cons "post_title" (or v "No Title")) wp))
              ((assq k org-blog-wp-alist)
               (cons (cons (cdr (assq k org-blog-wp-alist)) v) wp)))))
    post
    :initial-value nil)
   (lambda (a b)
     (string< (car a) (car b)))))

(defun org-blog-post-to-wp-add-taxonomy (wp taxonomy entries)
  "Handle adding taxonomy items to a WordPress struct.

The fiddly part is making sure that the sublists are sorted, for
convenience in testing and inspection."
  (let* ((terms (assoc "terms_names" wp))
         (existing (cdr terms))
         (struct (cons taxonomy entries)))
    (if existing
        (setcdr terms (sort
                       (cons struct existing)
                       (lambda (a b)
                         (string< (car a) (car b)))))
      (push (list "terms_names" struct) wp))
    wp))

(defun org-blog-wp-to-post (wp)
  "Transform a WordPress struct into a post.

This is largely about mapping tag names, though the `terms'
structure benefits from a helper function to handle mapping it
properly.

For convenience in testing and inspection, the resulting alist is
sorted."
  (sort
   (cl-reduce
    (lambda (post new)
      "Do key and value transformations."
      (let ((k (car new))
            (v (cdr new)))
        (cond ((eq v nil)
               post)
              ((string= "terms" k)
               (org-blog-wp-to-post-handle-taxonomy post v))
              ((string= "post_date_gmt" k)
               (cons (cons (car (rassoc k org-blog-wp-alist)) (plist-get v :datetime)) post))
              ((rassoc k org-blog-wp-alist)
               (cons (cons (car (rassoc k org-blog-wp-alist)) v) post))
              (t
               post))))
    wp
    :initial-value nil)
   (lambda (a b)
     (string< (car a) (car b)))))

(defun org-blog-wp-to-post-handle-taxonomy (post entries)
  "Handle mapping WordPress taxonomy info into a post struct.

We have to operate on all of the items in the taxonomy structure,
glomming them onto the existing post."
  (let* ((tlist (org-blog-wp-xml-terms-to-term-alist entries))
         (category (assoc "category" tlist))
         (tag (assoc "post_tag" tlist)))
    (when category
      (push (cons :category (cdr category)) post))
    (when tag
      (push (cons :keywords (cdr tag)) post))))

(defun org-blog-wp-xml-terms-to-term-alist (terms)
  "Handle turning WordPress taxonomy lists into an alist.

From here we can extract just the bits we need."
  (cl-reduce
   (lambda (lists term)
     (let ((name (cdr (assoc "name" term)))
           (taxonomy (cdr (assoc "taxonomy" term))))
       (cons (append (list taxonomy) (cdr (assoc taxonomy lists)) (list name)) lists)))
   terms
   :initial-value nil))

(defun org-blog-wp-params (blog)
  "Construct the basic paramlist for wordpress calls.

This starts with the information the user may have set for the
blog in their configuration, and then attempts to fill in any
holes so it can produce a list of necessearily generic
parameters.  `org-blog-wp-call' can then use the output of this
function to make other calls."
  (let ((complete (list (cons :engine "wp"))))
    (push (cons :xmlrpc (or (cdr (assq :xmlrpc blog))
                            (empty-string-is-nil (read-from-minibuffer "XML-RPC URL: "))
                            (error "Posting cancelled")))
          complete)

    (push (cons :username (or (cdr (assq :username blog))
                              (empty-string-is-nil (read-from-minibuffer "Username: "))
                              (error "Posting cancelled")))
          complete)
    (push (cons :password (or (cdr (assq :password blog))
                              (empty-string-is-nil (read-passwd "Password: "))
                              (error "Posting cancelled")))
          complete)
    (push (cons :blog-id (or (cdr (assq :blog-id blog))
                             (empty-string-is-nil (let ((userblogs (xml-rpc-method-call
                                                                    (cdr (assq :xmlrpc complete))
                                                                    'wp.getUsersBlogs
                                                                    (cdr (assq :username complete))
                                                                    (cdr (assq :password complete)))))
                                                    (cond
                                                     ;; If there's no blogs, fail
                                                     ((eq userblogs nil)
                                                      nil)
                                                     ;; If there's only one blog, use its blog-id (and xml-rpc) automatically
                                                     ((equal (length userblogs) 1)
                                                      (setcdr (assq :xmlrpc complete) (cdr (assoc "xmlrpc" (car userblogs))))
                                                      (cdr (assoc "blogid" (car userblogs))))
                                                     ;; If there's mroe than one blog, ask the user to choose from them
                                                     (t
                                                      (cl-reduce
                                                       (lambda (chosen entry)
                                                         (when (string= (cdr (assoc "blogName" result)) (cdr (assoc "blogName" entry)))
                                                           (setcdr (assq :xmlrpc complete) (cdr (assoc "xmlrpc" entry)))
                                                           (cdr (assoc "blogid" entry))))
                                                       userblogs
                                                       :initial-value (completing-read
                                                                       "Blog Name: "
                                                                       (mapcar (lambda (entry)
                                                                                 (cdr (assoc "blogName" entry)))
                                                                               userblogs) nil t))))))
                             (error "Posting cancelled")))
          complete)
    (sort
     complete
     (lambda (a b)
       (string< (car a) (car b))))))

;;;; RPC entry point

(defun org-blog-wp-call (blog call &rest args)
  "Easy calls to WordPress API functions.

This routine will take whatever information a user has available,
fill in the rest (if the user is willing to cooperate) and then
call the specified function and return the results."
  (let ((blog-id (cdr (assq :blog-id blog)))
        (func (intern (concat "org-blog-wp-" call)))
        (password (cdr (assq :password blog)))
        (username (cdr (assq :username blog)))
        (xmlrpc (cdr (assq :xmlrpc blog))))
    (if (fboundp func)
        (apply func xmlrpc blog-id username password args)
      (error "Can't find function %s" func))))

(defun org-blog-wp-post-create (xmlrpc blog-id username password post)
  "Create a post on the blog."
  (xml-rpc-method-call xmlrpc
                       'wp.newPost
                       blog-id
                       username
                       password
                       (org-blog-post-to-wp post)))


(defun org-blog-wp-post-retrieve (xmlrpc blog-id username password post-id &optional fields)
  "Fetches a single post from the weblog system."
  (let ((params (list xmlrpc
                      'wp.getPost
                      blog-id
                      username
                      password
                      post-id)))
    (when fields
      (append params (list fields) nil))
    (apply 'xml-rpc-method-call params)))

(defun org-blog-wp-post-update (xmlrpc blog-id username password post-id post)
  "Edit an existing post on the blog."
  (xml-rpc-method-call xmlrpc
                       'wp.editPost
                       blog-id
                       username
                       password
                       post-id
                       (org-blog-post-to-wp post))
  post-id)

(provide 'org-blog-wp)
;;; org-blog-wp.el ends here
