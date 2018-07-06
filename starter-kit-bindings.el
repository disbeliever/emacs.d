;;; starter-kit-bindings.el --- Set up some handy key bindings
;;
;; Part of the Emacs Starter Kit.

;; Like everywhere else, C-w kills word or region.
(global-set-key (kbd "C-w") 'backward-kill-word-or-kill-region)

(define-key minibuffer-local-map (kbd "C-w") 'backward-kill-word-or-kill-region)

(add-hook 'ido-setup-hook 
          (lambda ()
            (define-key ido-completion-map (kbd "C-w") 'ido-delete-backward-word-updir)))

(global-set-key [f2] 'save-buffer)

;; You know, like Readline.
(global-set-key (kbd "C-M-h") 'backward-kill-word)

;; Align your code in a pretty way.
(global-set-key (kbd "C-x \\") 'align-regexp)

;; Completion that uses many different methods to find options.
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "S-SPC") 'dabbrev-expand)

;; Perform general cleanup.
(global-set-key (kbd "C-c n") 'cleanup-buffer)

;; Turn on the menu bar for exploring new modes
(global-set-key (kbd "C-<f10>") 'menu-bar-mode)

;; Font size
;; (define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map [C-kp-add] 'text-scale-increase)
(define-key global-map [C-kp-subtract] 'text-scale-decrease)

;; Use regex searches by default.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "\C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; Jump to a definition in the current file. (This is awesome.)
(global-set-key (kbd "C-x C-i") 'ido-imenu)

;; File finding
(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
(global-set-key (kbd "C-x C-M-f") 'find-file-in-project)
(global-set-key (kbd "C-x f") 'recentf-ido-find-file)
(global-set-key (kbd "C-c y") 'bury-buffer)
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "M-`") 'file-cache-minibuffer-complete)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Window switching. (C-x o goes to the next window)
(windmove-default-keybindings) ;; Shift+direction
(global-set-key (kbd "C-x O") (lambda () (interactive) (other-window -1))) ;; back one
;(global-set-key (kbd "C-x C-o") (lambda () (interactive) (other-window 2))) ;; forward two

;; Start eshell or switch to it if it's active.
(global-set-key (kbd "C-x m") 'eshell)

;; Start a new eshell even if one is active.
(global-set-key (kbd "C-x M") (lambda () (interactive) (eshell t)))

;; Start a regular shell if you prefer that.
(global-set-key (kbd "C-x M-m") 'shell)

;; If you want to be able to M-x without meta (phones, etc)
(global-set-key (kbd "C-x C-m") 'execute-extended-command)

;; Fetch the contents at a URL, display it raw.
(global-set-key (kbd "C-x C-h") 'view-url)

;; Help should search more than just commands
(global-set-key (kbd "C-h a") 'apropos)

;; Should be able to eval-and-replace anywhere.
(global-set-key (kbd "C-c e") 'eval-and-replace)

;; For debugging Emacs modes
(global-set-key (kbd "C-c p") 'message-point)

;; So good!
(global-set-key (kbd "C-x g") 'magit-status)

;; This is a little hacky since VC doesn't support git add internally
(eval-after-load 'vc
  (define-key vc-prefix-map "i" '(lambda () (interactive)
                                   (if (not (eq 'Git (vc-backend buffer-file-name)))
                                       (vc-register)
                                     (shell-command (format "git add %s" buffer-file-name))
                                     (message "Staged changes.")))))

;; Activate occur easily inside isearch
(define-key isearch-mode-map (kbd "C-o")
  (lambda () (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string (regexp-quote isearch-string))))))

;; Org
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)

;; Switch between active buffers
(global-set-key [C-next] 'previous-buffer)
(global-set-key [C-prior] 'next-buffer)

(global-set-key [M-kp-left] 'windmove-left)      ; move to left windnow
(global-set-key [M-kp-right] 'windmove-right)    ; move to right window
(global-set-key [M-kp-up] 'windmove-up)          ; move to upper window
(global-set-key [M-kp-down] 'windmove-down)      ; move to downer window
(global-set-key [M-s-left] 'windmove-left)      ; move to left windnow
(global-set-key [M-s-right] 'windmove-right)    ; move to right window
(global-set-key [M-s-up] 'windmove-up)          ; move to upper window
(global-set-key [M-s-down] 'windmove-down)      ; move to downer window

(global-set-key (kbd "C-=") 'calc-eval-region)

;; global keybindings
(global-set-key (kbd "C-M-s") 'swiper)
(global-set-key [C-delete] 'geosoft-kill-buffer) 
(global-set-key [C-kp-delete] 'geosoft-kill-buffer) 
(global-set-key (kbd "C-x C-u") 'uim-mode)
(global-set-key [(control meta ?r)] 'remember)
(global-set-key [f8] 'compile)
;(global-set-key [f9] (lambda () (interactive) (shell-command ("./main"))))
;(global-set-key [f13] 'toggle-input-method)
(global-set-key (kbd "RET") 'reindent-then-newline-and-indent)
(global-set-key (kbd "ESC M-g") 'disbeliever/google-it)
(global-set-key [f10] (lambda () (interactive) (org-agenda) (delete-other-windows)))
(global-set-key [C-f12] (lambda () (interactive) (org-todo-list "TODO") (delete-other-windows)))
(global-set-key [M-s-left] 'windmove-left)      ; move to left windnow
(global-set-key [M-s-right] 'windmove-right)    ; move to right window
(global-set-key [M-s-up] 'windmove-up)          ; move to upper window
(global-set-key [M-s-down] 'windmove-down)      ; move to downer window
(define-key global-map "\C-cc" 'org-capture)
(global-set-key (kbd "ESC M-l") 'org-annotate-file)

;; Compose Key
(define-key key-translation-map [Multi_key] ; See `iso-transl.el'.
  (lookup-key key-translation-map "\C-x8"))

(provide 'starter-kit-bindings)
;;; starter-kit-bindings.el ends here
