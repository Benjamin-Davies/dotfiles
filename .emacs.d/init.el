(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives
	       '("melpa" . "http://melpa.milkbox.net/packages/")
	       t))

(package-initialize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (auctex evil))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Install packages
(package-install 'evil)
(package-install 'auctex)
(package-install 'linum-relative)

;; Set cursor and visual-bell
(set-default 'cursor-type 'hbar)
(setq visible-bell nil
      ring-bell-function (lambda ()
			   (invert-face 'mode-line)
			   (run-with-timer 0.1 nil #'invert-face 'mode-line)))

;;; Asthetics
;; Use different font size for mac
(if (eq system-type 'darwin)
    (add-to-list 'default-frame-alist
		 '(font . "Input Mono-15"))
  (add-to-list 'default-frame-alist
	       '(font . "Input Mono-11")))
(load-theme 'wombat)
(invert-face 'mode-line)

;;; Disable GUI
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;;; Functionality modes
(evil-mode)
(show-paren-mode)
(electric-pair-mode)

;;; Line numbers
(linum-mode)
(linum-relative-global-mode)
(setq linum-relative-current-symbol "")

;;; Modeline modes
(column-number-mode)
(display-time-mode)
(setq display-time-day-and-date t
      display-time-24hr-format t)
