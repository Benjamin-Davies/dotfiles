;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;;; Code:

(defvar IS-TERMUX (numberp (cl-search "com.termux" (getenv "PREFIX"))))

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Benjamin Davies"
      user-mail-address "bentendavies@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec
                 :family "Fira Code"
                 :size 16)
      doom-variable-pitch-font (font-spec
                                :family "Fira Sans"
                                :size 16))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(defvar my/light-theme 'doom-one-light)
(defvar my/dark-theme 'doom-gruvbox)
(defvar my/oled-theme 'doom-homage-black)

(setq doom-theme my/light-theme)

(when IS-TERMUX
  (setq doom-theme my/oled-theme))

(when (and IS-LINUX (not IS-TERMUX))
  (defun my/theme/follow-gnome-color-scheme (color-scheme)
    "Use the given color scheme."
    (cond
         ((string-match-p color-scheme "default")
          (load-theme my/light-theme))
         ((string-match-p color-scheme "prefer-dark")
          (load-theme my/dark-theme))
         (t (message "I don't know how to handle scheme: %s" color-scheme))))

  (defun my/theme/handle-dbus-event (namespace key value)
    "Handler for FreeDesktop theme changes."
    (when (and (string= namespace "org.gnome.desktop.interface") (string= key "color-scheme"))
      (my/theme/follow-gnome-color-scheme (car value))))

  (require 'dbus)

  ;; (let ((color-scheme (dbus-call-method :session "org.freedesktop.portal" "/org/freedesktop/portal/desktop"
  ;;                                       "org.freedesktop.impl.portal.Settings" "Read"
  ;;                                       "org.gnome.desktop.interface" "color-scheme")))
  ;;   (my/theme/follow-gnome-color-scheme color-scheme))

  (dbus-register-signal :session "org.freedesktop.portal" "/org/freedesktop/portal/desktop"
                        "org.freedesktop.impl.portal.Settings" "SettingChanged"
                        #'my/theme/handle-dbus-event))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/notes/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; Hide window borders
;; (when IS-LINUX
;;   (add-to-list 'default-frame-alist '(undecorated . t)))

;; Use native fullscreen on macOS
(when IS-MAC
  (setq ns-use-native-fullscreen t))

(use-package! dired-subtree)

(map! :n "C-s" #'save-buffer)

(map! :map org-mode-map
      :leader
      :prefix ("d" . "org-drill")
      "a" #'org-drill-again
      "c" #'org-drill-cram
      "C" #'org-drill-cram-tree
      "d" #'org-drill
      "D" #'org-drill-directory
      "r" #'org-drill-resume
      "t" #'org-drill-tree)

(map! :map doom-leader-toggle-map
      "C" #'company-mode)

(use-package! telega
  :init
  (setq telega-server-libs-prefix "/usr"))

;; Workaround for Org-Mode LaTeX Preview size
;; https://karthinks.com/software/scaling-latex-previews-in-emacs/

;; (setq org-preview-latex-default-process 'dvisvgm)
                                        ;No blur when scaling

(defun my/text-scale-adjust-latex-previews (&optional arg)
  "Adjust the size of latex preview fragments when changing the buffer's text scale."
  (pcase major-mode
    ('latex-mode
     (dolist (ov (overlays-in (point-min) (point-max)))
       (if (eq (overlay-get ov 'category)
               'preview-overlay)
           (my/text-scale--resize-fragment ov))))
    ('org-mode
     (dolist (ov (overlays-in (point-min) (point-max)))
       (if (eq (overlay-get ov 'org-overlay-type)
               'org-latex-overlay)
           (my/text-scale--resize-fragment ov))))))

(defun my/text-scale--resize-fragment (ov)
  (overlay-put
   ov 'display
   (cons 'image
         (plist-put
          (cdr (overlay-get ov 'display))
          :scale (+ 1.0 (* 0.25 text-scale-mode-amount))))))

(add-hook 'text-scale-mode-hook #'my/text-scale-adjust-latex-previews)
;; Added by me
(advice-add 'org-latex-preview :after #'my/text-scale-adjust-latex-previews)

;; And from the comments
;; Scales it by an additional 75%
(setq my/org-latex-scale 1.75)
(setq org-format-latex-options (plist-put org-format-latex-options :scale my/org-latex-scale))
