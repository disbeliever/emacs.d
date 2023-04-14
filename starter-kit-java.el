;;; starter-kit-java.el --- Java support

;;; Commentary:

;; 

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'starter-kit-java)

;;; Code:

(provide 'starter-kit-java)
(eval-when-compile
  (require 'cl))

(require 'company-lsp)
(require 'hydra)
(require 'projectile)
(require 'lsp-java)
(require 'lsp-ui)

(add-hook 'java-mode-hook 'lsp)



;;;;##########################################################################
;;;;  User Options, Variables
;;;;##########################################################################



;;; starter-kit-java.el ends here
