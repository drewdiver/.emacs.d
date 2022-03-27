;; (setq inhibit-startup-message t)

(setq user-full-name "Drew Diver"
      user-email-address "shout@drewdiver.com")

(scroll-bar-mode -1) ; disable visible scrollbar
(tool-bar-mode -1) ; disable the toolbar
(tooltip-mode -1) ; disable tooltips
(set-fringe-mode -1) ; extra room
(blink-cursor-mode -1)
(fset 'yes-or-no-p 'y-or-n-p)

(setq-default tab-width 4
              indent-tabs-mode nil) ; tabs are spaces

(load-theme 'wombat)

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Disable line highlight for the following modes...
(global-hl-line-mode 1) ; highlight current line
(dolist (mode '(eshell-mode-hook))
  (add-hook mode (lambda () (global-hl-line-mode 0))))

(global-display-line-numbers-mode 1)

;; Disable line numbers for the following modes...
(dolist (mode '(eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(global-set-key (kbd "C-x k") 'kill-this-buffer) ; kill current buffer
(setq prelude-use-smooth-scrolling t)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit) ; use esc to quit prompts
(setq gc-cons-threshold 50000000)
(setq large-file-warning-threshold 100000000)

;; I don't think we actually need this...
; (prefer-coding-system 'utf-8)
; (set-default-coding-systems 'utf-8)
; (set-terminal-coding-system 'utf-8)
; (set-keyboard-coding-system 'utf-8)

(require 'server)
(if (not (server-running-p)) (server-start))

(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :config
  (progn
    (require 'smartparens-config)
    (smartparens-global-mode 1)
    (show-paren-mode t)))

(use-package swiper :ensure t) ; needed for ivy

(use-package ivy
  :ensure t
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)	
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

;; needed to run M-x all-the-icons-install-fonts
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("ced4e3d3440ba3a74bbb2b107ba9e973373b5c656dcfd37c1ac7298cd974daf0" default))
 '(package-selected-packages '(use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-line ((t (:inherit highlight :extend t :underline nil)))))
