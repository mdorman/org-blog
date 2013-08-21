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
(require 'ox)
(require 'ox-html)
(require 'org-blog)

(eval-when-compile
  (require 'cl))

(defconst org-blog-buffer-options-alist
  (reduce
   (lambda (l i)
     (let ((field (plist-get (cdr i) :to-buffer)))
       (if (string-prefix-p "POST_" field t)
           (cons (list (car i) field nil nil t) l)
         l)))
   org-blog-post-mapping
   :initial-value nil))

(org-export-define-derived-backend 'blog 'html
  :filters-alist '((:filter-final-output . org-blog-filter-tag-newline)
                   (:filter-plain-text . org-blog-filter-text-newlines))
  :options-alist org-blog-buffer-options-alist
  :translate-alist '((link . org-blog-translate-link)))

;;; Filters
(defun org-blog-filter-tag-newline (content backend info)
  "Remove superfluous leading space and trailing newlines from tags

TREE is the parse tree being exported.  BACKEND is the export
back-end used.  INFO is a plist used as a communication channel.

Assume BACKEND is `blog'."
  ;; <tag>, </tag>, <tag/>, (replace-regexp-in-string "\\(<\\([[:alpha:]]+\\|/[[:alpha:]]+\\|[[:alpha:]]+/\\)>\\)\n+" "\\1" content)
  (replace-regexp-in-string "\s*\\(<[^>]+>\\)\n+" "\\1" content))

(defun org-blog-translate-link (link content info)
  "Fixup links"
  ;; (print (format "link is: %s\ncontent is: %s\ninfo is: %s\n" link content info))
  (let ((type (org-element-property :type link)))
    (cond ((member type '("custom-id" "id"))
           (let ((destination (org-export-resolve-id-link link info)))
             (format "<a href=\"%s\">%s</a>" destination contents)))
          ((equal type "fuzzy")
           ;; This is not ideal
           (let ((destination (org-element-property :path link)))
             (format "<a href=\"%s\">%s</a>" destination contents))))))

(defun org-blog-filter-text-newlines (content backend info)
  "Remove superfluous newlines in elements (except verse blocks)

TREE is the parse tree being exported.  BACKEND is the export
back-end used.  INFO is a plist used as a communication channel.

Assume BACKEND is `blog'."
  (cond ((eq 'verse-block (car (org-export-get-parent content)))
         content)
        (t
         (replace-regexp-in-string "\n" " " content))))

(defun org-blog-buffer-extract-post ()
  "Transform a buffer into a post.

We do as little processing as possible on individual items, to
retain the maximum flexibility for further transformation."
  (let ((content
         (org-export-as 'blog nil nil t '(:preserve-breaks nil
                                          :section-numbers nil
                                          :with-tags nil
                                          :with-toc nil
                                          :with-todo-keywords nil)))
        (attrs
         (org-export-get-environment 'blog)))
    (sort
     (reduce
      (lambda (l i)
        (let* ((v (plist-get attrs (car i)))
               (filter (plist-get (cdr i) :from-buffer))
               (value (if filter
                          (funcall filter v attrs)
                        v)))
          ;; We should only cons if there's a v and the output of the filter is non-nil
          (if value
              (cons (cons (car i) value) l)
            l)))
      org-blog-post-mapping
      :initial-value (when content
                       (list (cons :content content))))
     (lambda (a b)
       (string< (car a) (car b))))))

(defun org-blog-buffer-merge-post (merge)
  "Merge a post into a buffer.

Given a post structure (presumably returned from the server),
update the buffer to reflect the values it contains."
  (save-excursion
    (save-restriction
      ;; Get the current values
      (let ((current (org-blog-buffer-extract-post)))
        (mapc
         (lambda (item)
           (let ((k (car item))
                 (v (cdr item))
                 val existing)
             (when (cdr (assq k org-blog-post-mapping))
               (setq val (cond ((eq v nil)
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
               (cond
                ;; Inserting a new keyword
                ((eq (cdr (assq k current)) nil)
                 (when val
                   (insert (concat "#+" (plist-get (cdr (assq k org-blog-post-mapping)) :to-buffer) ": " val "\n"))))
                ;; Updating an existing keyword
                ((not (equal (cdr (assq k current)) val))
                 (let ((re (org-make-options-regexp (list (plist-get (cdr (assq k org-blog-post-mapping)) :to-buffer)) nil))
                       (case-fold-search t))
                   (re-search-forward re nil t)
                   (replace-match (concat "#+" (plist-get (cdr (assq k org-blog-post-mapping)) :to-buffer) ": " val) t t)))))))
         ;; Reverse sort fields to insert alphabetically
         (sort
          (copy-alist merge)
          (lambda (a b)
            (string< (car b) (car a)))))))))

;;;; Declare tests if ert is loaded
(when (featurep 'ert)
  (ert-deftest ob-test-extract-from-empty ()
    "Try extracting a post from an empty buffer."
    (with-temp-buffer
      (should (equal (org-blog-buffer-extract-post) nil))))

  (ert-deftest ob-test-extract-from-buffer ()
    "Try extracting a post from a buffer with stuff set."
    (with-temp-buffer
      (let ((post-string "\
#+POST_BLOG: t1b
#+POST_CATEGORY: t1c1, t1c2
#+DATE: [2013-01-25 Fri 00:00]
#+DESCRIPTION: t1e
#+POST_ID: 1
#+KEYWORDS: t1k1, t1k2, t1k3
#+POST_LINK: http://example.com/
#+POST_NAME: t1n
#+TITLE: Test 1 Title
#+POST_TYPE: post

Just a little bit of content.
There is still part of the paragraph.  Line breaks are refolded.

#+BEGIN_VERSE
Though the material in verse should
retain
its line
breaks
#+END_VERSE

[[org-blog-buffer.el][There's a link in here, too]]
")
            (post-struct '((:blog . "t1b")
                           (:category "t1c1" "t1c2")
                           (:content . "<p>Just a little bit of content. There is still part of the paragraph.  Line breaks are refolded.</p><p class=\"verse\">Though the material in verse should<br/>retain<br/>its line<br/>breaks<br/></p><p><a href=\"org-blog-buffer.el\">There's a link in here, too</a></p>")
                           (:date 20738 4432)
                           (:description . "t1e")
                           (:id . "1")
                           (:keywords "t1k1" "t1k2" "t1k3")
                           (:link . "http://example.com/")
                           (:name . "t1n")
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
                           (:content)
                           (:date)
                           (:description)
                           (:id)
                           (:keywords)
                           (:link)
                           (:name)
                           (:parent)
                           (:status)
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
#+KEYWORDS: t1k1, t1k2, t1k3
#+POST_LINK: http://example.com/
#+POST_NAME: t1n
#+POST_PARENT: 0
#+POST_STATUS: publish
#+TITLE: Test 1 Title
#+POST_TYPE: post
")
            (post-struct '((:blog . "t1b")
                           (:category "t1c1" "t1c2")
                           (:date 20738 4432)
                           (:description . "t1e")
                           (:id . "1")
                           (:keywords "t1k1" "t1k2" "t1k3")
                           (:link . "http://example.com/")
                           (:name . "t1n")
                           (:parent . "0")
                           (:status . "publish")
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
#+KEYWORDS: t2k1, t2k2, t2k3
#+POST_LINK: http://example.com/
#+POST_NAME: t2n
#+POST_PARENT: 0
#+POST_STATUS: publish
#+TITLE: Test 2 Title
#+POST_TYPE: post
")
            (post-struct '((:blog . "t2b")
                           (:category "t2c1" "t2c2")
                           (:date 20738 4432)
                           (:description . "t2e")
                           (:id . "1")
                           (:keywords "t2k1" "t2k2" "t2k3")
                           (:link . "http://example.com/")
                           (:name . "t2n")
                           (:parent . "0")
                           (:status . "publish")
                           (:title . "Test 2 Title")
                           (:type . "post"))))
        (org-blog-buffer-merge-post post-struct)
        (should (equal (buffer-string) post-string))
        (should (equal (org-blog-buffer-extract-post) post-struct))
        (org-blog-buffer-merge-post post-struct)
        (should (equal (buffer-string) post-string))))))
