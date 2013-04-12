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

(require 'org-blog-buffer)

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
    (org-blog-buffer-merge-post `((:blog . ,name)
                                  (:category . "")
                                  (:date ,(current-time))
                                  (:excerpt . "")
                                  (:format . "post")
                                  (:status . "publish")
                                  (:tags . "")
                                  (:title . "")
                                  (:type . "post")))))

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
)
