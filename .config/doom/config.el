;;; .doom.d/config.el -*- lexical-binding: t; -*-

; Fonts
(setq doom-font (font-spec :family "Input Mono" :size 16)
      doom-big-font (font-spec :family "Input Mono" :size 36)
      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 16)

      projectile-project-search-path '("~/Projects/"))

; Fullscreen on macOS
(if (eq system-type 'darwin)
    (toggle-frame-fullscreen))

; EXWM
(require 'exwm)
(require 'exwm-config)

;; Initial workspace number
(setq exwm-workspace-number 1)
;; Make class name the buffer name
(add-hook 'exwm-update-class-hook
          (lambda ()
            (exwm-workspace-rename-buffer exwm-class-name)))

(defun start (cmd)
  (lambda ()
    (interactive)
    (start-process-shell-command cmd nil cmd)))
(defun rofi (mode)
  (lambda ()
    (interactive)
    (shell-command "Rofi" nil (concat "rofi -i -show " mode " &"))))

;; Global keybindings
(setq exwm-input-global-keys
      `(
        ;; WM
        ([?\s-r] . exwm-reset)
        ([?\s-w] . exwm-workspace-switch)

        ;; Applications
        ([?\s-r] . (lambda (command)
                       (interactive (list (read-shell-command "$ ")))
                       (start-process-shell-command command nil command)))
        ([?\s- ] . ,(rofi "drun"))
;        ,@(app-bindings
;           `(([?\s-c] . "google-chrome-stable --new-window")
;             ([?\s-e] . "thunar")))

        ;; Workspaces
        ,@(mapcar (lambda (i)
                    `(,(kbd (format "s-%d" i)) .
                      (lambda ()
                        (interactive)
                        (exwm-workspace-switch-create ,i))))
                  (number-sequence 0 9))))

(define-key exwm-mode-map (kbd "C-c") nil)
(define-key exwm-mode-map [?\s-.] 'exwm-input-send-next-key)

;; Use some stuff from the default config
(exwm-enable)
(exwm-config-ido)
(exwm-config-misc)
