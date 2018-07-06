;; (add-hook 'haskell-mode-hook
;;           (lambda ()
;;             (unless (or (file-exists-p "makefile")
;;                         (file-exists-p "Makefile"))
;;               (set (make-local-variable 'compile-command)
;;                    (concat "ghc --make "
;;                            (buffer-file-name)
;;                            " -o "
;;                            (file-name-sans-extension (buffer-file-name)))))
;;             ))

(add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'haskell-font-lock-symbols t)


(provide 'starter-kit-haskell)
