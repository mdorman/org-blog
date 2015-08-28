(require 'dash)

(add-to-list 'load-path ".")

(require 'org-blog)

(describe "org-blog"

  (before-all
    (-if-let (password (getenv "PASSWORD"))
        (setq org-blog-test-password (getenv "PASSWORD"))))
  
  (describe "org-blog-mode"
    (it "can turn on the mode"
      (with-temp-buffer
        (org-blog-mode)
        (expect org-blog-mode :to-be t))))

  (describe "org-blog-get-name"
    (it "can get the blog name from a blog spec"
      (-let [name (org-blog-get-name '((:blog . "foo")))]
        (expect name :to-equal "foo")))

    (it "can get the blog name from an alist"
      (-let [org-blog-alist '(("bar"))]
        (expect (org-blog-get-name) :to-equal "bar")))

    (it "can get the blog name from completing-read"
      (spy-on 'completing-read :and-return-value "baz")
      (expect (org-blog-get-name) :to-equal "baz"))

    (it "can get the blog name from default"
      (spy-on 'completing-read :and-return-value "")
      (expect (org-blog-get-name) :to-equal "unknown")))

  (describe "org-blog-new"
    (before-each
      (spy-on 'current-time :and-return-value (current-time)))
    (after-each
      (kill-buffer (current-buffer)))
  
    (it "can create a new blog post from an alist"
      (let ((org-blog-alist '(("bar")))
            (post-string (concat "\
#+POST_BLOG: bar
#+POST_CATEGORY: 
#+DATE: [" (format-time-string "%Y-%m-%d %a %R") "]
#+DESCRIPTION: 
#+KEYWORDS: 
#+POST_STATUS: publish
#+TITLE: 
#+POST_TYPE: post
")))
        (org-blog-new)
        (expect (org-no-properties (buffer-string)) :to-equal post-string)))

    (it "can create a new blog post using completing-read"
      (spy-on 'completing-read :and-return-value "baz")
      (let ((post-string (concat "\
#+POST_BLOG: baz
#+POST_CATEGORY: 
#+DATE: [" (format-time-string "%Y-%m-%d %a %R") "]
#+DESCRIPTION: 
#+KEYWORDS: 
#+POST_STATUS: publish
#+TITLE: 
#+POST_TYPE: post
")))
        (org-blog-new)
        (expect (org-no-properties (buffer-string)) :to-equal post-string)))

    (it "can createa new blog post with a default"
      (spy-on 'completing-read :and-return-value "bar")
      (let ((org-blog-alist '(("bar")))
            (post-string (concat "\
#+POST_BLOG: bar
#+POST_CATEGORY: 
#+DATE: [" (format-time-string "%Y-%m-%d %a %R") "]
#+DESCRIPTION: 
#+KEYWORDS: 
#+POST_STATUS: publish
#+TITLE: 
#+POST_TYPE: post
")))
        (org-blog-new)
        (expect (org-no-properties (buffer-string)) :to-equal post-string))))

  (describe "org-blog-post-to-blog"

    (after-each
      (kill-buffer (current-buffer)))

    (it "can get the blog information from a blog post"
      (unless (boundp 'org-blog-test-password)
        (signal 'buttercup-pending t))
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
        (expect (org-blog-post-to-blog (org-blog-buffer-extract-post)) :to-equal final-blog-param))))

  (describe "org-blog-save"
    (it "can transfer from buffers to posts and back again"
      (unless (boundp 'org-blog-test-password)
        (signal 'buttercup-pending t))
      (let* ((blog (org-blog-wp-params `((:blog-id . 46183217)
                                         (:directory . "~/org/blogging")
                                         (:engine . "wp")
                                         (:password . ,org-blog-test-password)
                                         (:username . "mdorman@ironicdesign.com")
                                         (:xmlrpc . "https://orgblogtest.wordpress.com/xmlrpc.php"))))
             (org-blog-alist (list (cons "testing" blog))))
        (with-temp-buffer
          (insert (concat "\
#+POST_BLOG: testing
#+POST_CATEGORY: testing, repetitious
#+DATE: [" (format-time-string "%Y-%m-%d %a %R") "]
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
            (expect pre-save :to-equal (buffer-string))))))))
