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
                                    (:xmlrpc . "https://wordpress.com/xmlrpc.php")
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
         (should (equal pre-save (buffer-string))))))))
