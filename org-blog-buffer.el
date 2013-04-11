;;; org-blog-buffer.el --- Operate on org-blog buffers
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

(provide 'org-blog-buffer)

(require 'org)
(require 'org-exp)

(defun org-blog-buffer-extract-post ()
  "Transform a buffer into a post.

We do as little processing as possible on individual items, to
retain the maximum flexibility for further transformation."
  (save-excursion
    (save-restriction
      (let ((org-export-inbuffer-options-extra '(("POST_BLOG" :blog)
                                                 ("POST_CATEGORY" :category)
                                                 ("POST_ID" :id)
                                                 ("POST_LINK" :link)
                                                 ("POST_NAME" :name)
                                                 ("POST_PARENT" :parent)
                                                 ("POST_STATUS" :status)
                                                 ("POST_TYPE" :type)))
            (org-export-date-timestamp-format "%Y%m%dT%T%z")
            (org-export-with-preserve-breaks nil)
            (org-export-with-priority nil)
            (org-export-with-section-numbers nil)
            (org-export-with-sub-superscripts nil)
            (org-export-with-tags nil)
            (org-export-with-toc nil)
            (org-export-with-todo-keywords nil))
        (sort
         (list (cons :blog (property-trim :blog))
               (cons :category (property-split :category))
               (cons :date (let ((timestamp (property-trim :date)))
                             (when timestamp
                               (list (date-to-time timestamp)))))
               (cons :excerpt (property-trim :description))
               (cons :id (property-trim :id))
               (cons :link (property-trim :link))
               (cons :name (property-trim :name))
               (cons :parent (property-trim :parent))
               (cons :status (property-trim :status))
               (cons :tags (property-split :keywords))
               (cons :title (property-trim :title))
               (cons :type (property-trim :type))
               (cons :content (org-no-properties (condition-case nil
                                                     (org-export-as-html nil nil nil 'string t nil)
                                                   (wrong-number-of-arguments
                                                    (org-export-as-html nil nil 'string t nil))))))
         '(lambda (a b)
            (string< (car a) (car b))))))))

(defun property-trim (k)
  "Get a property value trimmed of leading spaces."
  (let ((v (plist-get (org-infile-export-plist) k)))
    (when v
      (replace-regexp-in-string "^[[:space:]]+" "" v))))

(defun property-split (k)
  "Get a property value trimmed of leading spaces and split on commas."
  (let ((v (property-trim k)))
    (when v
      (split-string v "\\( *, *\\)" t))))

;;;; Run tests if ert is loaded
(when (featurep 'ert)
  (ert-deftest ob-test-extract-from-empty ()
    "Try extracting a post from an empty buffer."
    (with-temp-buffer
      (should (equal (org-blog-buffer-extract-post) '((:blog)
                                                      (:category)
                                                      (:content . "\n")
                                                      (:date)
                                                      (:excerpt)
                                                      (:id)
                                                      (:link)
                                                      (:name)
                                                      (:parent)
                                                      (:status)
                                                      (:tags)
                                                      (:title)
                                                      (:type))))))

  (ert-deftest ob-test-extract-from-buffer ()
    "Try extracting a post from a buffer with stuff set."
    (with-temp-buffer
      (insert "\
#+POST_BLOG: t1b
#+POST_CATEGORY: t1c1, t1c2
#+DATE: [2013-01-25 Fri 00:00]
#+DESCRIPTION: t1e
#+POST_ID: 1
#+POST_LINK: http://example.com/
#+POST_NAME: t1n
#+KEYWORDS: t1k1, t1k2, t1k3
#+TITLE: Test 1 Title
#+POST_TYPE: post

Just a little bit of content.")
      (should (equal (org-blog-buffer-extract-post) '((:blog . "t1b")
                                                      (:category "t1c1" "t1c2")
                                                      (:content . "\n<p>Just a little bit of content.\n</p>")
                                                      (:date (20738 4432))
                                                      (:excerpt . "t1e")
                                                      (:id . "1")
                                                      (:link . "http://example.com/")
                                                      (:name . "t1n")
                                                      (:parent)
                                                      (:status)
                                                      (:tags "t1k1" "t1k2" "t1k3")
                                                      (:title . "Test 1 Title")
                                                      (:type . "post")))))))
