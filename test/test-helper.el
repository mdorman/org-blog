(require 'el-mock)
(require 'ert)
(require 'f)

(defvar org-blog-test/test-path
  (f-dirname load-file-name))

(defvar org-blog-test/root-path
  (f-parent org-blog-test/test-path))

(add-to-list 'load-path org-blog-test/root-path)

(setq org-blog-test-password (getenv "PASSWORD")
      stack-trace-on-error t
      xml-rpc-debug 10)

(unless org-blog-test-password
  (error "You must give the secret password in the PASSWORD env variable"))

(require 'org-blog)
