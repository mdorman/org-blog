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

;;;; Define tests if ert is loaded
(when (featurep 'ert)
  (setq message-log-max t)
  (ert-deftest ob-test-enable-org-blog-mode ()
    "Test turning on the org-blog minor mode"
    (with-temp-buffer
      (org-blog-mode)
      (should (eq org-blog-mode t))))
