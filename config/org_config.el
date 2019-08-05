(require 'org)

(add-hook 'org-babel-after-execute-hook
	  (lambda () (org-redisplay-inline-images)))
