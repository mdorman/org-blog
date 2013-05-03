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
  "Get a property split on commas."
  (when v
    (split-string (org-blog-property-strip v i) "\\( *, *\\)" t)))

(defun org-blog-property-strip (v i)
  "Strip properties from a property string."
  (when v
    (set-text-properties 0 (length v) nil v)
    v))

(defun org-blog-date-format (v i)
  "Properly format a date."
  (date-to-time
   (org-export-get-date i "%Y%m%dT%T%z")))

(defconst org-blog-post-mapping '((:blog :attr "POST_BLOG" :from-buffer org-blog-property-strip)
                                  (:category :attr "POST_CATEGORY" :from-buffer org-blog-property-split)
                                  (:date :attr "DATE" :from-buffer org-blog-date-format)
                                  (:description :attr "DESCRIPTION" :from-buffer org-blog-property-strip)
                                  (:id :attr "POST_ID" :from-buffer org-blog-property-strip)
                                  (:keywords :attr "KEYWORDS" :from-buffer org-blog-property-split)
                                  (:link :attr "POST_LINK" :from-buffer org-blog-property-strip)
                                  (:name :attr "POST_NAME" :from-buffer org-blog-property-strip)
                                  (:parent :attr "POST_PARENT" :from-buffer org-blog-property-strip)
                                  (:status :attr "POST_STATUS" :from-buffer org-blog-property-strip)
                                  (:title :attr "TITLE" :from-buffer (lambda (v i) (org-blog-property-strip (car v) i)))
                                  (:type :attr "POST_TYPE" :from-buffer org-blog-property-strip)))

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
  (setq message-log-max t
        test-time (current-time))
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
      (stub current-time => test-time)
      (let ((org-blog-alist '(("bar")))
            (post-string (concat "\
#+POST_BLOG: bar
#+POST_CATEGORY: 
#+DATE: [" (format-time-string "%Y-%m-%d %a %R" (current-time)) "]
#+DESCRIPTION: 
#+KEYWORDS: 
#+POST_STATUS: publish
#+TITLE: 
#+POST_TYPE: post
")))
        (org-blog-new)
        (should (string= (org-no-properties (buffer-string)) post-string)))))
  (ert-deftest ob-test-org-blog-new-from-completing-read ()
    "Test creating a new blog post using completing-read"
    (with-mock
      (stub current-time => test-time)
      (stub completing-read => "baz")
      (let ((post-string (concat "\
#+POST_BLOG: baz
#+POST_CATEGORY: 
#+DATE: [" (format-time-string "%Y-%m-%d %a %R" (current-time)) "]
#+DESCRIPTION: 
#+KEYWORDS: 
#+POST_STATUS: publish
#+TITLE: 
#+POST_TYPE: post
")))
        (org-blog-new)
        (should (string= (org-no-properties (buffer-string)) post-string)))))
  (ert-deftest ob-test-org-blog-new-from-default ()
    "Test creating a new blog post with a default"
    (with-mock
      (stub current-time => test-time)
      (stub completing-read => "")
      (let ((org-blog-alist '(("bar")))
            (post-string (concat "\
#+POST_BLOG: bar
#+POST_CATEGORY: 
#+DATE: [" (format-time-string "%Y-%m-%d %a %R" (current-time)) "]
#+DESCRIPTION: 
#+KEYWORDS: 
#+POST_STATUS: publish
#+TITLE: 
#+POST_TYPE: post
")))
        (org-blog-new)
        (should (string= (org-no-properties (buffer-string)) post-string)))))

  (ert-deftest ob-test-org-blog-post-to-blog ()
    "Test getting the blog information from a blog post"
    (let ((org-blog-alist `(("bar" . ((:engine . "wp")
                                      (:xmlrpc . "http://wordpress.com/xmlrpc.php")
                                      (:username . "mdorman@ironicdesign.com")
                                      (:password . ,org-blog-test-password)))))
          (final-blog-param `((:blog-id . "46183217")
                              (:engine . "wp")
                              (:password . ,org-blog-test-password)
                              (:username . "mdorman@ironicdesign.com")
                              (:xmlrpc . "https://orgblogtest.wordpress.com/xmlrpc.php"))))
      (org-blog-new)
      (should (equal (org-blog-post-to-blog (org-blog-buffer-extract-post)) final-blog-param))))

  (ert-deftest ob-test-org-blog-save ()
    "Transfer from buffers to posts and back again"
    (let* ((debug-on-error 1)
           (blog (org-blog-wp-params `((:blog-id . 46183217)
                                       (:directory . "~/org/blogging")
                                       (:engine . "wp")
                                       (:password . ,org-blog-test-password)
                                       (:username . "mdorman@ironicdesign.com")
                                       (:xmlrpc . "https://orgblogtest.wordpress.com/xmlrpc.php"))))
           (org-blog-alist (list (cons "testing" blog))))
      (with-mock
        (stub current-time => test-time)
        (with-temp-buffer
          (insert (concat "\
#+POST_BLOG: testing
#+POST_CATEGORY: testing, repetitious
#+DATE: [" (format-time-string "%Y-%m-%d %a %R" (current-time)) "]
#+DESCRIPTION: This is an automated test-post
#+KEYWORDS: testing, automation, emacs rocks
#+POST_STATUS: publish
#+TITLE: Testing, testing, 1, 2, 3, 4
#+POST_TYPE: post

There's *really* not much to see here.  This is an automated post
for testing org-blog, so we're really just focussed on whether it
works at all, not the content of the post.

I do want some paragraph breaks
and some odd
short
lines that should necessarily end up being part of a continuous paragraph.

#+BEGIN_VERSE
Though the material in verse should
retain
its line
breaks
#+END_VERSE
"))
          (org-blog-save)
          (goto-char (point-max))
          (insert "\n\nThis is a little additional text")
          (let ((pre-save (buffer-string)))
            (org-blog-save)
            (should (equal pre-save (buffer-string)))))))))
