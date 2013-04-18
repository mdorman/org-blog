;;; org-blog.el --- Manage a blog using org-mode
;;; -*- lexical-binding: t -*-
;;
;; Copyright (C) 2013 Michael Alan Dorman
;;
;; Author: Michael Alan Dorman <mdorman at ironicdesign dot com>
;; Keywords: blog, org-mode
;;
;; This file is part of org-blog.
;;
;; org-blog is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; org-blog is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; GNU General Public License at <http://www.gnu.org/licenses/>.

(provide 'org-blog)

(define-minor-mode org-blog-mode
  "Toggle org-blog mode.

With no argument, the mode is toggled on/off.
Non-nil argument turns mode on.
Nil argument turns mode off.

Commands:

\\{org-blog-mode-map}"
  :init-value nil
  :lighter " org-blog")

(defcustom org-blog-alist nil
  "An alist for specifying blog information.

There are a number of parameters.  Some day I will enumerate
them.")

(defvar org-blog-engine-alist nil
  "A list of back-ends we support.

Each loaded back-end should add its name to the list.")

(defconst org-blog-post-mapping '((:blog :org "POST_BLOG")
                                  (:category :org "POST_CATEGORY")
                                  (:date :org "DATE")
                                  (:excerpt :org "DESCRIPTION")
                                  (:id :org "POST_ID")
                                  (:link :org "POST_LINK")
                                  (:name :org "POST_NAME")
                                  (:parent :org "POST_PARENT")
                                  (:status :org "POST_STATUS")
                                  (:tags :org "KEYWORDS")
                                  (:title :org "TITLE")
                                  (:type :org "POST_TYPE")))

(require 'org-blog-buffer)
(require 'org-blog-wp)

(defun org-blog-new ()
  "Create a new buffer primed for a blog entry.

If the user has only one blog defined in the `org-blog-alist',
that blog will be used by default, otherwise the user will be
prompted to chose the blog for which the post is intended.  The
user can choose to enter a name that is not configured if they
desire, and when they go to save the buffer, they will be
prompted for information on how to post to the blog.

The new buffer is initialized with the name of the blog, a
timestamp reflecting the current time, and a number of other
empty fields that the user may wish to fill in."
  (interactive)
  (let ((name (org-blog-get-name)))
    (switch-to-buffer (generate-new-buffer (format "*org-blog/%s*" name)))
    (org-mode)
    (org-blog-mode)
    ;; FIXME: Get defaults from org-blog-alist entry for the blog
    (org-blog-buffer-merge-post (list (cons :blog name)
                                      (cons :category "")
                                      (cons :date (current-time))
                                      (cons :excerpt "")
                                      (cons :format "post")
                                      (cons :status "publish")
                                      (cons :tags "")
                                      (cons :title "")
                                      (cons :type "post")))))

(defun org-blog-get-name (&optional post)
  "Get a name of a blog, perhaps associated with a post.

Given a post structure, we will extract the blog name from it.
Otherwise, if there's only one entry in the `org-blog-alist', we
will use that entry by default, but will accept anything, as long
as the user confirms it, and if they don't enter anything at all,
we default to unknown."
  (or (cdr (assoc :blog post))
      (and (equal (length org-blog-alist) 1)
           (caar org-blog-alist))
      (empty-string-is-nil (completing-read
                            "Blog to post to: "
                            (mapcar 'car org-blog-alist) nil 'confirm))
      "unknown"))

(defun empty-string-is-nil (string)
  "Return any string except the empty string, which is coerced to nil."
  (unless (= 0 (length string))
    string))

(defun org-blog-call (blog call &rest args)
  "Make the specified call to the appropriate blog engine.

This allows us to maintain multiple engines, with a set of
operations common to all, and call the appropriate function based
on the engine specification in the entry in `org-blog-alist'."
  (let ((entry (intern (concat "org-blog-" (cdr (assq :engine blog)) "-call"))))
    (if (fboundp entry)
        (apply entry blog call args)
      (error (format  "Can't find function %s" entry)))))

(defun org-blog-post-to-blog (post)
  "Determine the blog to use for the given post.

It will ask for the blog name and blog engine if necessary, and
then hand off to the particular engine's `-params' function, so
it may make a number of interactive queries to the user."
  (let* ((name (org-blog-get-name post))
         (blog (cdr (assoc name org-blog-alist)))
         (engine (org-blog-blog-to-engine blog))
         (funcname (concat "org-blog-" engine "-params"))
         (func (intern funcname)))
    (unless (functionp func)
      (error (format "Can't find params function for %s engine" engine)))
    (apply func blog nil)))

(defun org-blog-blog-to-engine (blog)
  "Get the blog engine name from the blog structure.

If it's not present, ask the user to choose from among those
available in org-blog-alist."
  (let ((engine (or (cdr (assq :engine blog))
                    (empty-string-is-nil (completing-read
                                          "Blog software: "
                                          (mapcar 'car org-blog-engine-alist) nil t)))))
    (unless engine
      (error (format "Can't find engine %s" engine)))
    engine))

;;;; Define tests if ert is loaded
(when (featurep 'ert)
  (require 'el-mock)
  (setq message-log-max t)
  (ert-deftest ob-test-enable-org-blog-mode ()
    "Test turning on the org-blog minor mode"
    (with-temp-buffer
      (org-blog-mode)
      (should (eq org-blog-mode t))))
  (ert-deftest ob-test-get-name-from-blog ()
    "Test getting the blog name from a blog spec"
    (should (string= (org-blog-get-name '((:blog . "foo"))) "foo")))
  (ert-deftest ob-test-get-name-from-alist ()
    "Test getting the blog name from the alist"
    (let ((org-blog-alist '(("bar"))))
      (should (string= (org-blog-get-name) "bar"))))
  (ert-deftest ob-test-get-name-from-completing-read ()
    "Test getting the blog name from completing-read"
    (with-mock
     (stub completing-read => "baz")
     (should (string= (org-blog-get-name) "baz"))))
  (ert-deftest ob-test-get-name-from-default ()
    "Test getting the blog name from default"
    (with-mock
     (stub completing-read => "")
     (should (string= (org-blog-get-name) "unknown"))))
  (ert-deftest ob-test-org-blog-new-from-alist ()
    "Test creating a new blog post with an alist"
    (with-mock
     (stub current-time => '(20738 4432))
     (let ((org-blog-alist '(("bar")))
           (post-string "\
#+POST_BLOG: bar
#+POST_CATEGORY: 
#+DATE: [2013-01-25 Fri 00:00]
#+DESCRIPTION: 
#+POST_STATUS: publish
#+KEYWORDS: 
#+TITLE: 
#+POST_TYPE: post
"))
       (org-blog-new)
       (should (string= (org-no-properties (buffer-string)) post-string)))))
  (ert-deftest ob-test-org-blog-new-from-completing-read ()
    "Test creating a new blog post using completing-read"
    (with-mock
     (stub current-time => '(20738 4432))
     (stub completing-read => "baz")
     (let ((post-string "\
#+POST_BLOG: baz
#+POST_CATEGORY: 
#+DATE: [2013-01-25 Fri 00:00]
#+DESCRIPTION: 
#+POST_STATUS: publish
#+KEYWORDS: 
#+TITLE: 
#+POST_TYPE: post
"))
       (org-blog-new)
       (should (string= (org-no-properties (buffer-string)) post-string)))))
  (ert-deftest ob-test-org-blog-new-from-default ()
    "Test creating a new blog post with a default"
    (with-mock
     (stub current-time => '(20738 4432))
     (stub completing-read => "")
     (let ((org-blog-alist '(("bar")))
           (post-string "\
#+POST_BLOG: bar
#+POST_CATEGORY: 
#+DATE: [2013-01-25 Fri 00:00]
#+DESCRIPTION: 
#+POST_STATUS: publish
#+KEYWORDS: 
#+TITLE: 
#+POST_TYPE: post
"))
       (org-blog-new)
       (should (string= (org-no-properties (buffer-string)) post-string)))))

  (ert-deftest ob-test-org-blog-post-to-blog ()
    "Test getting the blog information from a blog post"
    (let* ((blog-passwd (read-passwd "Password for blog listing: "))
           (org-blog-alist `(("bar" . ((:engine . "wp")
                                       (:xmlrpc . "http://wordpress.com/xmlrpc.php")
                                       (:username . "mdorman@ironicdesign.com")
                                       (:password . ,blog-passwd)))))
           (final-blog-param `((:blog-id . "46183217")
                               (:engine . "wp")
                               (:password . ,blog-passwd)
                               (:username . "mdorman@ironicdesign.com")
                               (:xmlrpc . "https://orgblogtest.wordpress.com/xmlrpc.php"))))
      (org-blog-new)
      (should (equal (org-blog-post-to-blog (org-blog-buffer-extract-post)) final-blog-param)))))
