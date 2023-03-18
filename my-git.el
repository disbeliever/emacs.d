(setq magit-auto-revert-mode nil)

(global-diff-hl-mode)
(eval-after-load 'magit
  '(progn
     (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
     (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)))

(provide 'my-git)
