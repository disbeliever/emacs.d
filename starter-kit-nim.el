(use-package nim-mode
  :init
  (add-hook 'nim-mode-hook
            (lambda ()
              ;; (setq-local compile-command "nimble build")
              (define-key nim-mode-map (kbd "<f8>") 'nimble--compile)
              (define-key nim-mode-map (kbd "C-<f8>") 'nimble--run)
              ;; (define-key nim-mode-map (kbd "M-.") 'dumb-jump-go)
              (define-key nim-mode-map (kbd "M-.") 'nimsuggest-find-definition)
              (diff-hl-mode)
              )))

(use-package nimble)

(provide 'starter-kit-nim)
