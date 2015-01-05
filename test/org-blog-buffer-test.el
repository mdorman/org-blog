;;;; Declare tests if ert is loaded
(when (featurep 'ert)
  (ert-deftest ob-test-extract-from-empty ()
    "Try extracting a post from an empty buffer."
    (should
     (equal (with-temp-buffer
              (org-blog-buffer-extract-post))
            nil)))

  (ert-deftest ob-test-extract-from-buffer ()
    "Try extracting a post from a buffer with stuff set."
    (should
     (equal (with-temp-buffer
              (insert "\
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

 [[org-blog-buffer.el][There's a link in here, too]]")
              (org-blog-buffer-extract-post))
            '((:blog . "t1b")
              (:category "t1c1" "t1c2")
              (:content . "<p>Just a little bit of content. There is still part of the paragraph.  Line breaks are refolded.</p><p class=\"verse\">Though the material in verse should<br  />retain<br  />its line<br  />breaks<br  /></p><p><a href=\"org-blog-buffer.el\">There's a link in here, too</a></p>")
              (:date 20738 4432)
              (:description . "t1e")
              (:id . "1")
              (:keywords "t1k1" "t1k2" "t1k3")
              (:link . "http://example.com/")
              (:name . "t1n")
              (:title . "Test 1 Title")
              (:type . "post")))))

  (ert-deftest ob-test-merge-from-empty ()
    "Try merging an empty post into an empty buffer."
    (should
     (equal (with-temp-buffer
              (org-blog-buffer-merge-post  '((:blog)
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
                                             (:type)))
              (buffer-string))
            "")))

  (ert-deftest ob-test-merge-from-full ()
    "Try merging a full post into an empty buffer."
    (should
     (equal (with-temp-buffer
              (org-blog-buffer-merge-post '((:blog . "t1b")
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
                                            (:type . "post")))
              (buffer-string))
            "\
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
")))

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
