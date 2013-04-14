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
                               (date-to-time timestamp))))
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
         #'(lambda (a b)
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

(defconst mapping (list (cons :blog "POST_BLOG")
                        (cons :category "POST_CATEGORY")
                        (cons :date "DATE")
                        (cons :excerpt "DESCRIPTION")
                        (cons :id "POST_ID")
                        (cons :link "POST_LINK")
                        (cons :name "POST_NAME")
                        (cons :parent "POST_PARENT")
                        (cons :status "POST_STATUS")
                        (cons :tags "KEYWORDS")
                        (cons :title "TITLE")
                        (cons :type "POST_TYPE")))

(defun org-blog-buffer-merge-post (merge)
  "Merge a post into a buffer.

Given a post structure (presumably returned from the server),
update the buffer to reflect the values it contains."
  (save-excursion
    (save-restriction
      ;; Get the current values
      (let ((current (org-blog-buffer-extract-post)))
        (mapc
         #'(lambda (item)
           (let ((k (car item))
                 (v (cdr item))
                 val existing)
             (when (cdr (assq k mapping))
               (setq val (cond ((eq v nil)
                                (print "setting val to nil")
                                nil)
                               ((eq k :date)
                                (format-time-string "[%Y-%m-%d %a %H:%M]" v))
                               ((listp v)
                                (mapconcat 'identity v ", "))
                               ((stringp v) 
                                v)
                               (t
                                "default")))
               (goto-char (point-min))
               ;; (print (format "Comparison for %s is %s against %s" k v (cdr (assq k current))))
               (cond
                ;; Inserting a new keyword
                ((eq (cdr (assq k current)) nil)
                 (when val
                   (insert (concat "#+" (cdr (assq k mapping)) ": " val "\n"))))
                ;; Updating an existing keyword
                ((not (equal (cdr (assq k current)) val))
                 (let ((re (org-make-options-regexp (list (cdr (assq k mapping))) nil))
                       (case-fold-search t))
                   (re-search-forward re nil t)
                   (replace-match (concat "#+" (cdr (assq k mapping)) ": " val) t t)))))))
         ;; Reverse sort fields to insert alphabetically
         (sort
          (copy-alist merge)
          #'(lambda (a b)
	      (string< (car b) (car a)))))))))

;;;; Declare tests if ert is loaded
(when (featurep 'ert)
  (ert-deftest ob-test-extract-from-empty ()
    "Try extracting a post from an empty buffer."
    (with-temp-buffer
      (let ((post-string "")
            (post-struct '((:blog)
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
                           (:type))))
        (should (equal (org-blog-buffer-extract-post) post-struct)))))

  (ert-deftest ob-test-extract-from-buffer ()
    "Try extracting a post from a buffer with stuff set."
    (with-temp-buffer
      (let ((post-string "\
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
            (post-struct '((:blog . "t1b")
                           (:category "t1c1" "t1c2")
                           (:content . "\n<p>Just a little bit of content.\n</p>")
                           (:date 20738 4432)
                           (:excerpt . "t1e")
                           (:id . "1")
                           (:link . "http://example.com/")
                           (:name . "t1n")
                           (:parent)
                           (:status)
                           (:tags "t1k1" "t1k2" "t1k3")
                           (:title . "Test 1 Title")
                           (:type . "post"))))
      (insert post-string)
      (should (equal (org-blog-buffer-extract-post) post-struct)))))

  (ert-deftest ob-test-merge-from-empty ()
    "Try merging an empty post into an empty buffer."
    (with-temp-buffer
      (let ((post-string "")
            (post-struct '((:blog)
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
                           (:type))))
        (org-blog-buffer-merge-post post-struct)
        (should (equal (buffer-string) post-string)))))

  (ert-deftest ob-test-merge-from-full ()
    "Try merging a full post into an empty buffer."
    (with-temp-buffer
      (let ((post-string "\
#+POST_BLOG: t1b
#+POST_CATEGORY: t1c1, t1c2
#+DATE: [2013-01-25 Fri 00:00]
#+DESCRIPTION: t1e
#+POST_ID: 1
#+POST_LINK: http://example.com/
#+POST_NAME: t1n
#+POST_PARENT: 0
#+POST_STATUS: publish
#+KEYWORDS: t1k1, t1k2, t1k3
#+TITLE: Test 1 Title
#+POST_TYPE: post
")
            (post-struct '((:blog . "t1b")
                           (:category "t1c1" "t1c2")
                           (:content . "\n")
                           (:date 20738 4432)
                           (:excerpt . "t1e")
                           (:id . "1")
                           (:link . "http://example.com/")
                           (:name . "t1n")
                           (:parent . "0")
                           (:status . "publish")
                           (:tags "t1k1" "t1k2" "t1k3")
                           (:title . "Test 1 Title")
                           (:type . "post"))))
        (org-blog-buffer-merge-post post-struct)
        (should (equal (buffer-string) post-string)))))

  (ert-deftest ob-test-merge-round-trip ()
    "Try merging a full post into a full buffer, and make sure
you get the same thing out."
    (with-temp-buffer
      (let ((post-string "#+POST_BLOG: t2b
#+POST_CATEGORY: t2c1, t2c2
#+DATE: [2013-01-25 Fri 00:00]
#+DESCRIPTION: t2e
#+POST_ID: 1
#+POST_LINK: http://example.com/
#+POST_NAME: t2n
#+POST_PARENT: 0
#+POST_STATUS: publish
#+KEYWORDS: t2k1, t2k2, t2k3
#+TITLE: Test 2 Title
#+POST_TYPE: post
")
            (post-struct '((:blog . "t2b")
                           (:category "t2c1" "t2c2")
                           (:content . "\n")
                           (:date 20738 4432)
                           (:excerpt . "t2e")
                           (:id . "1")
                           (:link . "http://example.com/")
                           (:name . "t2n")
                           (:parent . "0")
                           (:status . "publish")
                           (:tags "t2k1" "t2k2" "t2k3")
                           (:title . "Test 2 Title")
                           (:type . "post"))))
        (org-blog-buffer-merge-post post-struct)
        (should (equal (buffer-string) post-string))
        (should (equal (org-blog-buffer-extract-post) post-struct))
        (org-blog-buffer-merge-post post-struct)
        (should (equal (buffer-string) post-string))))))
