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
(exwm-config-default)
