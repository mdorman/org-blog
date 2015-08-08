;;; org-blog.el --- Manage a blog using org-mode
;;; -*- lexical-binding: t -*-

;; Copyright (C) 2013 Michael Alan Dorman

;; Author: Michael Alan Dorman <mdorman at ironicdesign dot com>
;; Created: Feb 5 2013
;; Homepage: https://github.com/mdorman/org-blog
;; Keywords: blog, org, org-mode
;; Package-Requires: ((org "8.3") (xml-rpc "1.6.12"))
;; Version: 0.15

;; This file is not part of GNU Emacs.

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

;;; Commentary:

;; The purpose of org-blog is to allow you to use org-mode markup to
;; create content for one or more blogs, keep local copies of
;; articles, synchronize local and remote copies as much as is
;; sensible, and do it all from within emacs with a minimum of fuss.

;;; Code:

(require 'org)
(require 'org-blog-buffer)
(require 'org-blog-wp)

;;;### autoload
(define-minor-mode org-blog-mode
  "Toggle org-blog mode.

With no argument, the mode is toggled on/off.
Non-nil argument turns mode on.
Nil argument turns mode off.

Commands:

\\{org-blog-mode-map}"
  :init-value nil
  :keymap '(([?\C-c ?\C-b ?p] . org-blog-save))
  :lighter " org-blog")

(defcustom org-blog-alist nil
  "An alist for specifying blog information.

There are a number of parameters.  Some day I will enumerate
them.")

(defvar org-blog-engine-alist nil
  "A list of back-ends we support.

Each loaded back-end should add its name to the list.")

(defun org-blog-property-split (v i)
  "Split a property V on commas.

We only have the argument I because the calling convention needs
to accomodate `org-blog-date-format'"
  (when v
    (split-string (org-blog-property-strip v i) "\\( *, *\\)" t)))

(defun org-blog-property-strip (v i)
  "Strip properties from a property string V.

We only have the argument I because the calling convention needs
to accomodate `org-blog-date-format'"
  (when v
    ;; If we got a list, only do the head
    (when (listp v)
      (message "Saw list value for %s" v)
      (setq v (car v)))
    ;; (message "Setting text properties on %s" v)
    (let ((clean (substring-no-properties v)))
      ;; (message "Doing string match on %s" v)
      (unless (string-match "^\s*$" clean)
        ;; (message "Found non-whitespace characters")
        clean))))

(defun org-blog-date-format (v i)
  "Properly format the document date extracted from I.

We only have the argument V because the calling convention needs
to accomodate `org-blog-property-split'
`org-blog-property-strip'"
  (when v
    (date-to-time
     (org-export-get-date i "%Y%m%dT%T%z"))))

(defun org-blog-title-format (value info)
  (let ((default (or (let ((visited-file (buffer-file-name (buffer-base-buffer))))
		   (and visited-file
			(file-name-sans-extension
			 (file-name-nondirectory visited-file))))
		 (buffer-name (buffer-base-buffer))))
        (val (org-element-interpret-data (plist-get info :title) info)))
    (cond ((equal default val)
  "Properly format a title VALUE, using export options INFO."
           nil)
          (t
           val))))

(defconst org-blog-post-mapping '((:blog :to-buffer "POST_BLOG" :from-buffer org-blog-property-strip)
                                  (:category :to-buffer "POST_CATEGORY" :from-buffer org-blog-property-split)
                                  (:date :to-buffer "DATE" :from-buffer org-blog-date-format)
                                  (:description :to-buffer "DESCRIPTION" :from-buffer org-blog-property-strip)
                                  (:id :to-buffer "POST_ID" :from-buffer org-blog-property-strip)
                                  (:keywords :to-buffer "KEYWORDS" :from-buffer org-blog-property-split)
                                  (:link :to-buffer "POST_LINK" :from-buffer org-blog-property-strip)
                                  (:name :to-buffer "POST_NAME" :from-buffer org-blog-property-strip)
                                  (:parent :to-buffer "POST_PARENT" :from-buffer org-blog-property-strip)
                                  (:status :to-buffer "POST_STATUS" :from-buffer org-blog-property-strip)
                                  (:title :to-buffer "TITLE" :from-buffer org-blog-title-format)
                                  (:type :to-buffer "POST_TYPE" :from-buffer org-blog-property-strip)))

(require 'org-blog-buffer)
(require 'org-blog-wp)

;;;### autoload
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
                                      (cons :description "")
                                      (cons :format "post")
                                      (cons :keywords "")
                                      (cons :status "publish")
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

(defun org-blog-save ()
  "Publish an article to a server and save locally.

By default org-blog will try and save the article in a heirarchy
that mirrors the permalink structure for the blog in question."
  (interactive)
  (condition-case failure
      (let* ((post (org-blog-buffer-extract-post))
             (blog (org-blog-post-to-blog post))
             (post-id (if (cdr (assq :id post))
                          (org-blog-call blog "post-update" (cdr (assq :id post)) post)
                        (org-blog-call blog "post-create" post)))
             (post (org-blog-call blog "post-retrieve" post-id)))
        (org-blog-buffer-merge-post (org-blog-wp-to-post post)))
    (error (apply 'message (cdr failure)))))

(defun org-blog-call (blog call &rest args)
  "Make the specified CALL to the appropriate BLOG engine.

This allows us to maintain multiple engines, with a set of
operations common to all, and call the appropriate function based
on the engine specification in the entry in `org-blog-alist'."
  (let ((entry (intern (concat "org-blog-" (cdr (assq :engine blog)) "-call"))))
    (if (fboundp entry)
        (apply entry blog call args)
      (error (format  "Can't find function %s" entry)))))

(defun org-blog-post-to-blog (post)
  "Determine the blog to use for the given POST.

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
  "Get the blog engine name from the BLOG structure.

If it's not present, ask the user to choose from among those
available in org-blog-alist."
  (let ((engine (or (cdr (assq :engine blog))
                    (empty-string-is-nil (completing-read
                                          "Blog software: "
                                          (mapcar 'car org-blog-engine-alist) nil t)))))
    (unless engine
      (error (format "Can't find engine %s" engine)))
    engine))

(provide 'org-blog)
;;; org-blog.el ends here
