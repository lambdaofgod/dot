(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless (package-installed-p 'evil)
  (package-install 'evil))
(package-activate 'evil)
(require 'evil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (company-auctex company-emacs-eclim company jedi evil python))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(evil-mode 1)

(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)       
(global-set-key [C-M-\]] 'jedi:goto-definition)

(add-hook 'after-init-hook 'global-company-mode)
(with-eval-after-load 'company
  (add-to-list 'company-backends 'company-math-symbols-unicode))
(custom-set-variables
    '(TeX-view-program-list (quote (("Qpdfview" "qpdfview --unique %o"))))
    '(TeX-view-program-selection
	(quote (((output-dvi has-no-display-manager)
	"dvi2tty")
	((output-dvi style-pstricks)
	"dvips and gv")
	(output-dvi "xdvi")
	(output-pdf "Qpdfview")
	(output-html "xdg-open")))))

(setq-default indent-tabs-mode nil)
(setq  tab-width  4)
