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

(defun split-and-follow-horizontally ()
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 2") 'split-and-follow-horizontally)

(defun split-and-follow-vertically ()
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 3") 'split-and-follow-vertically)

(defun goto-definition ()
  (interactive)
  (split-and-follow-vertically)
  (jedi:goto-definition))
(global-set-key "\C-\M-]" 'goto-definition)

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
(windmove-default-keybindings)
