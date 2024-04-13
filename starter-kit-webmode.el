(add-to-list 'auto-mode-alist '("\\.css$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php$" . web-mode))

(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-enable-auto-closing t)
(setq web-mode-enable-auto-opening t)
(setq web-mode-enable-auto-indentation t)

(require 'auto-complete)
(require 'php-auto-yasnippets)
(ac-config-default)
(payas/ac-setup)

(setq web-mode-ac-sources-alist
      '(("php" . (
                  ;ac-source-yasnippet
                  ac-source-php-auto-yasnippets
                  ;ac-source-dictionary
                  ))
        ("html" . (ac-source-emmet-html-aliases ac-source-emmet-html-snippets))
        ("css" . (ac-source-css-property ac-source-emmet-css-snippets))))

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (auto-complete-mode)
  (emmet-mode)

  (flycheck-select-checker my-php)
  (flycheck-mode t)
  
  ;; (define-key css-mode-map (kbd "TAB") 'auto-complete)

  ;; (define-key php-mode-map (kbd "TAB") 'yas/create-php-snippet)
  ;; (define-key html-mode-map (kdb "C-c j") 'emmet-expand-line)
  ;; (define-key html-mode-map (kdb "C-c j") 'emmet-expand-line)
)
(add-hook 'web-mode-hook 'my-web-mode-hook)

(add-hook 'web-mode-before-auto-complete-hooks
          #'(lambda ()
             (let ((web-mode-cur-language
                    (web-mode-language-at-pos)))
               (if (string= web-mode-cur-language "php")
                   (yas-activate-extra-mode 'php-mode)
                 (yas-deactivate-extra-mode 'php-mode)
                 ;; (define-key web-mode-map (kbd "TAB") 'yas/create-php-snippet)
                 )
               (if (string= web-mode-cur-language "css")
                   (setq emmet-use-css-transform t)
                 (setq emmet-use-css-transform nil)
                 )
               )))

(provide 'starter-kit-webmode)
