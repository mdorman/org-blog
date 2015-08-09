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
(require 'org-blog)
(require 'ox)
(require 'ox-html)

(eval-when-compile
  (require 'cl))

;; Construct the data structure to be handed to
;; org-export-define-derived-backend by folding over our
;; `org-blog-post-mapping' structure.

(defconst org-blog-buffer-options-alist
  (org-reduce
   (lambda (l i)
     (let ((field (plist-get (cdr i) :to-buffer)))
       ;; Only add our fields, distinguised by the POST_ prefix
       (if (string-prefix-p "POST_" field t)
           (cons (list (car i) field nil nil t) l)
         l)))
   org-blog-post-mapping
   :initial-value nil))

;;; Filters
(defun org-blog-filter-tag-newline (content backend info)
  "Remove superfluous leading space and trailing newlines from tags

TREE is the parse tree being exported.  BACKEND is the export
back-end used.  INFO is a plist used as a communication channel.

Assume BACKEND is `blog'."
  ;; <tag>, </tag>, <tag/>, (replace-regexp-in-string "\\(<\\([[:alpha:]]+\\|/[[:alpha:]]+\\|[[:alpha:]]+/\\)>\\)\n+" "\\1" content)
  (replace-regexp-in-string "\s*\\(<[^>]+>\\)\n+" "\\1" content))

(defun org-blog-filter-text-newlines (content backend info)
  "Remove superfluous newlines in elements (except verse blocks)

TREE is the parse tree being exported.  BACKEND is the export
back-end used.  INFO is a plist used as a communication channel.

Assume BACKEND is `blog'."
  (cond ((eq 'verse-block (car (org-export-get-parent content)))
         content)
        (t
         (replace-regexp-in-string "\n" " " content))))

(defun org-blog-translate-link (link content info)
  "Fixup links"
  (let ((type (org-element-property :type link)))
    (cond ((member type '("custom-id" "id"))
           (let ((destination (org-export-resolve-id-link link info)))
             (format "<a href=\"%s\">%s</a>" destination contents)))
          ((equal type "fuzzy")
           ;; This is not ideal
           (let ((destination (org-element-property :path link)))
             (format "<a href=\"%s\">%s</a>" destination contents))))))

(org-export-define-derived-backend 'blog 'html
  :filters-alist '((:filter-final-output . org-blog-filter-tag-newline)
                   (:filter-plain-text . org-blog-filter-text-newlines))
  :options-alist org-blog-buffer-options-alist
  :translate-alist '((link . org-blog-translate-link)))

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
     (org-reduce
      (lambda (l i)
        (let* ((v (plist-get attrs (car i)))
               (filter (plist-get (cdr i) :from-buffer))
               (value (if (and v
                               (not (= 0 (length v))))
                          (if filter
                              (funcall filter v attrs)
                            v))))
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
  ;; We should rewrite this to work through the parser interface
  (save-excursion
    (save-restriction
      ;; Get the current values
      (let ((current (org-blog-buffer-extract-post)))
        ;; Iterate over the stuff to merge in
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
                ;; No existing value associated with keyword
                ((eq (cdr (assq k current)) nil)
                 (when val
                   (insert (concat "#+" (plist-get (cdr (assq k org-blog-post-mapping)) :to-buffer) ": " val "\n"))))
                ;; Existing value associated with keyword does not match new value
                ((not (equal (cdr (assq k current)) val))
                 ;; Prepare to search for the keyword
                 (let ((re (org-make-options-regexp (list (plist-get (cdr (assq k org-blog-post-mapping)) :to-buffer)) nil))
                       (case-fold-search t))
                   (cond
                    ;; If it was found
                    ((re-search-forward re nil t)
                     (replace-match (concat "#+" (plist-get (cdr (assq k org-blog-post-mapping)) :to-buffer) ": " val) t t))
                    ;; It was not found
                    (val
                     (insert (concat "#+" (plist-get (cdr (assq k org-blog-post-mapping)) :to-buffer) ": " val "\n"))))))))))
         ;; Reverse sort fields to insert alphabetically
         (sort
          (copy-alist merge)
          (lambda (a b)
            (string< (car b) (car a)))))))))

(provide 'org-blog-buffer)
;;; org-blog-buffer.el ends here
