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
     (should (string= (org-blog-get-name) "unknown")))))
