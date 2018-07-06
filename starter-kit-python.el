;;; starter-kit-python.el --- Some helpful Python code
;;
;; Part of the Emacs Starter Kit
(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
(setq interpreter-mode-alist (cons '("python" . python-mode)
                                   interpreter-mode-alist))
(autoload 'python-mode "python-mode" "Python editing mode." t)

(eval-after-load 'python-mode
  '(progn
     (global-set-key (kbd "RET") 'reindent-then-newline-and-indent)))

(global-set-key (kbd "C-h p") 'python-shell)
(add-hook 'python-mode-hook 'idle-highlight)

;;; Flymake

(eval-after-load 'python-mode
  '(progn
     (require 'flymake)
     (when (load "flymake" t)
       (defun flymake-pylint-init ()
         (let* ((temp-file (flymake-init-create-temp-buffer-copy
                            'flymake-create-temp-inplace))
                (local-file (file-relative-name
                             temp-file
                             (file-name-directory buffer-file-name))))
           (list "epylint" (list local-file))))
       (add-to-list 'flymake-allowed-file-name-masks
                    '("\\.py\\'" flymake-pylint-init)))))

;;; Electric Pairs
(defun electric-pair ()
  "If at end of line, insert character pair without surrounding spaces.
   Otherwise, just insert the typed character."
  (interactive)
  (if (eolp) (let (parens-require-spaces) (insert-pair)) 
    (self-insert-command 1)))

(add-hook 'python-mode-hook
     (lambda ()
      (define-key python-mode-map "\"" 'electric-pair)
      (define-key python-mode-map "\'" 'electric-pair)
      (define-key python-mode-map "(" 'electric-pair)
      (define-key python-mode-map "[" 'electric-pair)
      (define-key python-mode-map "{" 'electric-pair)))

(autoload 'python-pep8 "python-pep8")

(defun python-compile ()
  ;; Use compile to run python programs
  (interactive)
  (compile (concat "python " (buffer-name))))

(defun my-python-hook ()
  (local-set-key (kbd "C-c C-c") 'python-compile)
  (local-set-key [f8] 'pep8))
(add-hook 'python-mode-hook 'my-python-hook)

;;; Auto completion
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

;;; pylookup
(add-to-list 'load-path "~/.emacs.d/custom-packages/pylookup/")
(eval-when-compile (require 'pylookup))
(setq pylookup-program "~/bin/pylookup.py")
(setq pylookup-db-file "~/.cache/pylookup.db")
(global-set-key [(control shift menu)] 'pylookup-lookup)

(provide 'starter-kit-python)
;; starter-kit-python.el ends here