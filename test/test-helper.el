(require 'dash)
(require 'el-mock)
(require 'ert)
(require 'f)

(defvar org-blog-test/test-path
  (f-dirname load-file-name))

(defvar org-blog-test/root-path
  (f-parent org-blog-test/test-path))

(add-to-list 'load-path org-blog-test/root-path)

(-if-let (password (getenv "PASSWORD"))
    (setq org-blog-test-password  (getenv "PASSWORD")
          xml-rpc-debug 10))

(require 'org-blog)
