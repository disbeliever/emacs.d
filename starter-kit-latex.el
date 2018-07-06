(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCtex t)

(setq TeX-parse-self t)
(setq-default TeX-master nil)



(defun alexott/TeX-keymap ()
  (local-set-key [(meta i)]
                 '(lambda ()
                    (interactive)
                    (insert "\n\\item "))))

(defun alexott/tex-mode-hook ()
  ;;(local-set-key "\\" 'TeX-electric-macro)
  ;;(turn-on-bib-cite)
  (alexott/TeX-keymap)
  (setq bib-cite-use-reftex-view-crossref t))
(add-hook 'TeX-mode-hook 'alexott/tex-mode-hook)
(add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)

(provide 'starter-kit-latex)
