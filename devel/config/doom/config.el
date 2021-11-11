;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


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
                  :size (if IS-MAC 15 13)
                  );:weight 'semi-light)
       doom-variable-pitch-font (font-spec
                                 :family "Fira Sans"
                                 :size (if IS-MAC 16 14)))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'dracula)

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
(when IS-LINUX
  (add-to-list 'default-frame-alist '(undecorated . t)))

;; Use native fullscreen on macOS
(when IS-MAC
  (setq ns-use-native-fullscreen t))

(defun bd/email-account (type label address &optional default?)
  "Helper function to simplify email account definitions"
  (set-email-account!
   label
   (flet ((folder (sub-folder)
                  (concat "/" address sub-folder)))
     (cl-case type
       (:gmail  `((mu4e-sent-folder     . ,(folder "/[Gmail]/Sent Mail"))
                  (mu4e-drafts-folder   . ,(folder "/[Gmail]/Drafts"))
                  (mu4e-trash-folder    . ,(folder "/[Gmail]/Bin"))
                  (mu4e-refile-folder   . ,(folder "/[Gmail]/All mail"))
                  (smtpmail-smtp-server . "smtp.gmail.com")
                  (smtpmail-smtp-user   . ,address)))
       (:office `((mu4e-sent-folder     . ,(folder "/Sent Items"))
                  (mu4e-drafts-folder   . ,(folder "/Drafts"))
                  (mu4e-trash-folder    . ,(folder "/Deleted Items"))
                  (mu4e-refile-folder   . ,(folder "/All mail"))
                  (smtpmail-smtp-server . "smtp.office365.com")
                  (smtpmail-smtp-user   . ,address)))))
   default?))

;; t disables errors, as this file is per-computer
;; (I don't need to publish my email settings)
(load "~/.config/doom/mail.el" t)

(defun bd/notes (&optional arg)
  "Open my notes folder"
  (interactive "P")
  (projectile-switch-project-by-name "~/notes/" arg))
(map! :leader :desc "Notes" :n "o n" #'bd/notes)
