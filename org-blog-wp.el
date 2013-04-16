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

(provide 'org-blog-wp)

(require 'org-blog)
(require 'xml-rpc)

(add-to-list 'org-blog-engine-alist '("wp"))

(eval-when-compile
  (require 'cl))

;;;; Transformation to and from a post

(defconst org-blog-wp-alist
  (list (cons :category "category")
        (cons :content "post_content")
        (cons :date "post_date_gmt")
        (cons :excerpt "post_excerpt")
        (cons :id "post_id")
        (cons :link "link")
        (cons :name "post_name")
        (cons :parent "post_parent")
        (cons :status "post_status")
        (cons :tags "post_tag")
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
   (reduce
    #'(lambda (wp new)
        (let ((k (car new))
              (v (cdr new)))
          (cond ((eq v nil)
                 wp)
                ((eq :category k)
                 (org-blog-post-to-wp-add-taxonomy wp "category" v))
                ((eq :date k)
                 ;; Convert to GMT by adding seconds offset
                 (cons
                  (cons "post_date_gmt" (list :datetime
                                              (time-add v
                                                        (seconds-to-time (- (car (current-time-zone)))))))
                  wp))
                ((eq :tags k)
                 (org-blog-post-to-wp-add-taxonomy wp "post_tag" v))
                ((eq :title k)
                 (cons (cons "post_title" (or v "No Title")) wp))
                ((assq k org-blog-wp-alist)
                 (cons (cons (cdr (assq k org-blog-wp-alist)) v) wp)))))
    post :initial-value nil)
   #'(lambda (a b)
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
                       #'(lambda (a b)
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
   (reduce
    #'(lambda (post new)
        "Do key and value transformations."
        (let ((k (car new))
              (v (cdr new)))
          (cond ((eq v nil)
                 post)
                ((string= "terms" k)
                 (org-blog-wp-to-post-handle-taxonomy post v))
                ((string= "post_date_gmt" k)
                 ;; Must be a better way to extract this value
                 (cons (cons (car (rassoc k org-blog-wp-alist)) (time-add (cadr v) (seconds-to-time (car (current-time-zone))))) post))
                ((rassoc k org-blog-wp-alist)
                 (cons (cons (car (rassoc k org-blog-wp-alist)) v) post))
                (t
                 post))))
    wp :initial-value nil)
   #'(lambda (a b)
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
      (push (cons :tags (cdr tag)) post))))

(defun org-blog-wp-xml-terms-to-term-alist (terms)
  "Handle turning WordPress taxonomy lists into an alist.

From here we can extract just the bits we need."
  (reduce
   #'(lambda (lists term)
       (let ((name (cdr (assoc "name" term)))
             (taxonomy (cdr (assoc "taxonomy" term))))
         (cons (append (list taxonomy) (cdr (assoc taxonomy lists)) (list name)) lists)))
   terms :initial-value nil))

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
                                                     ;; FIXME: Prompt the user from the list of blogs (if there's more than 1
                                                     ;; Then shove the blog info into complete
                                                     (t
                                                      (reduce
                                                       #'(lambda (entry)
                                                           (when (string= (cdr (assoc "blogName" entry)))
                                                             (print (format "XMLRPC from server is %s" (cdr (assoc "xmlrpc" userblog))))
                                                             (setcdr (assq :xmlrpc complete) (cdr (assoc "xmlrpc" userblog)))
                                                             (cdr (assoc "blogid" userblog))))
                                                       userblogs
                                                       :initial-value (completing-read
                                                                       "Blog Name: "
                                                                       (mapcar #'(lambda (entry)
                                                                                   (cdr (assoc "blogName" entry)))
                                                                               userblogs) nil t))))))
                             (error "Posting cancelled")))
          complete)
    (sort
     complete
     #'(lambda (a b)
         (string< (car a) (car b))))))

;;;; Define tests if ert is loaded
(when (featurep 'ert)
  (ert-deftest ob-test-post-to-wp ()
    "Transfer from buffers to posts and back again"
    (let ((post1-struct '((:blog . "t1b")
                          (:category "t1c1" "t1c2")
                          (:content . "\n<p>Test 1 Content\n</p>")
                          (:date 20738 4432 0 0)
                          (:excerpt . "t1e")
                          (:id . "1")
                          (:link . "http://example.com/")
                          (:name . "t1n")
                          (:parent . "0")
                          (:status . "publish")
                          (:tags "t1k1" "t1k2" "t1k3")
                          (:title . "Test 1 Title")
                          (:type . "post")))
          (post1-wp-input '(("link" . "http://example.com/")
                            ("post_content" . "\n<p>Test 1 Content\n</p>")
                            ("post_date_gmt" :datetime (20738 18832 0 0))
                            ("post_excerpt" . "t1e")
                            ("post_id" . "1")
                            ("post_name" . "t1n")
                            ("post_parent" . "0")
                            ("post_status" . "publish")
                            ("post_title" . "Test 1 Title")
                            ("post_type" . "post")
                            ("terms_names"
                             ("category" "t1c1" "t1c2")
                             ("post_tag" "t1k1" "t1k2" "t1k3")))))
      (should (equal (org-blog-post-to-wp post1-struct) post1-wp-input))))
  (ert-deftest ob-test-wp-to-post ()
    "Transform from a WP response to a post"
    (let ((post1-struct '((:blog . "t1b")
                          (:category "t1c1" "t1c2")
                          (:content . "\n<p>Test 1 Content\n</p>")
                          (:date 20738 4432 0 0)
                          (:excerpt . "t1e")
                          (:id . "1")
                          (:link . "http://example.com/")
                          (:name . "t1n")
                          (:parent . "0")
                          (:status . "publish")
                          (:tags "t1k1" "t1k2" "t1k3")
                          (:title . "Test 1 Title")
                          (:type . "post")))
          (post1-wp-output '(("post_id" . "1")
                             ("post_title" . "Test 1 Title")
                             ("post_date" :datetime
                              (20738 4432))
                             ("post_date_gmt" :datetime
                              (20738 18832 0 0))
                             ("post_modified" :datetime
                              (20738 4432))
                             ("post_modified_gmt" :datetime
                              (20738 4432))
                             ("post_status" . "publish")
                             ("post_type" . "post")
                             ("post_name" . "t1n")
                             ("post_author" . "3075621")
                             ("post_password")
                             ("post_excerpt" . "t1e")
                             ("post_content" . "\n<p>Test 1 Content\n</p>")
                             ("post_parent" . "0")
                             ("post_mime_type")
                             ("link" . "http://example.com/")
                             ("guid" . "http://example.com/")
                             ("menu_order" . 0)
                             ("comment_status" . "closed")
                             ("ping_status" . "open")
                             ("sticky")
                             ("post_thumbnail")
                             ("post_format" . "standard")
                             ("terms"
                              (("term_id" . "126039325")
                               ("name" . "t1c1")
                               ("slug" . "t1c1")
                               ("term_group" . "0")
                               ("term_taxonomy_id" . "4")
                               ("taxonomy" . "category")
                               ("description")
                               ("parent" . "0")
                               ("count" . 0))
                              (("term_id" . "126039469")
                               ("name" . "t1c2")
                               ("slug" . "t1c2")
                               ("term_group" . "0")
                               ("term_taxonomy_id" . "5")
                               ("taxonomy" . "category")
                               ("description")
                               ("parent" . "0")
                               ("count" . 0))
                              (("term_id" . "147991082")
                               ("name" . "t1k1")
                               ("slug" . "t1k1")
                               ("term_group" . "0")
                               ("term_taxonomy_id" . "6")
                               ("taxonomy" . "post_tag")
                               ("description")
                               ("parent" . "0")
                               ("count" . 0))
                              (("term_id" . "147991085")
                               ("name" . "t1k2")
                               ("slug" . "t1k2")
                               ("term_group" . "0")
                               ("term_taxonomy_id" . "7")
                               ("taxonomy" . "post_tag")
                               ("description")
                               ("parent" . "0")
                               ("count" . 0))
                              (("term_id" . "147991087")
                               ("name" . "t1k3")
                               ("slug" . "t1k3")
                               ("term_group" . "0")
                               ("term_taxonomy_id" . "8")
                               ("taxonomy" . "post_tag")
                               ("description")
                               ("parent" . "0")
                               ("count" . 0)))
                             ("custom_fields"))))
      ;; FIXME: we should actually be looking up :blog in the alist
      (should (equal (cons (cons :blog "t1b") (org-blog-wp-to-post post1-wp-output)) post1-struct))))

  (ert-deftest ob-test-wp-params ()
    "Test getting the blog-id (and correct xmlrpc URL) via xmlrpc"
    (let* ((blog-passwd (read-passwd "Password for blog listing: "))
           (initial-blog-param `((:xmlrpc . "http://wordpress.com/xmlrpc.php")
                                 (:username . "mdorman@ironicdesign.com")
                                 (:password . ,blog-passwd)))
           (final-blog-param `((:blog-id . "46183217")
                               (:engine . "wp")
                               (:password . ,blog-passwd)
                               (:username . "mdorman@ironicdesign.com")
                               (:xmlrpc . "https://orgblogtest.wordpress.com/xmlrpc.php"))))
      (should (equal (org-blog-wp-params initial-blog-param) final-blog-param)))))
