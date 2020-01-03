(setq doom-font (font-spec :family "input mono" :size 16)
      doom-big-font (font-spec :family "input mono" :size 36)
      doom-variable-pitch-font (font-spec :family "fira sans" :size 16))

(setq doom-theme 'doom-one-light)

(setq org-bullets-bullet-list '("\u200B"))

(defun my/org-mode-hook ()
  (dolist (pair '((org-level-1 . 1.6)
                  (org-level-2 . 1.5)
                  (org-level-3 . 1.4)
                  (org-level-4 . 1.3)
                  (org-level-5 . 1.2)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car pair) nil :foreground "black" :height (cdr pair))))

(add-hook 'org-mode-hook 'my/org-mode-hook)

(add-hook 'org-mode-hook 'mixed-pitch-mode)

(setq projectile-project-search-path '("~/Projects/"))

(if (eq system-type 'darwin)

    (toggle-frame-fullscreen))
