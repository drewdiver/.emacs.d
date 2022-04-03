(setq inhibit-startup-message t)

(setq user-full-name "Drew Diver"
      user-email-address "shout@drewdiver.com")

(scroll-bar-mode -1) ; disable visible scrollbar
(tool-bar-mode -1) ; disable the toolbar
(tooltip-mode -1) ; disable tooltips
(set-fringe-mode -1) ; extra room
(setq visible-bell 1) ; silence
(blink-cursor-mode -1) ; are you blind?
(fset 'yes-or-no-p 'y-or-n-p) ; lazy

(setq-default tab-width 4
              indent-tabs-mode nil) ; tabs are spaces

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

;; (load-theme 'wombat)
;; I really like flatwhite, plain, tokyo-night
(use-package doom-themes
  :ensure t
  :defer
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;; (load-theme 'doom-plain-dark t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config))

;; To see where that is, use C-h v then type temporary-file-directory and hit enter.
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Disable line highlight for the following modes...
(global-hl-line-mode 1) ; highlight current line
(dolist (mode '(eshell-mode-hook))
  (add-hook mode (lambda () (global-hl-line-mode 0))))

(global-display-line-numbers-mode 1)

;; Disable line numbers for the following modes...
(dolist (mode '(eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(setq-default fill-column 80)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'markdown-mode 'turn-on-auto-fill)
(global-set-key (kbd "C-x k") 'kill-this-buffer) ; kill current buffer
(setq prelude-use-smooth-scrolling t)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit) ; use esc to quit prompts
(setq gc-cons-threshold 50000000)
(setq large-file-warning-threshold 100000000)
(global-set-key (kbd "C-c C-v") 'view-mode) ; enable view-mode

;; Get correct behaviour from macOS
(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(require 'server)
(if (not (server-running-p)) (server-start))

(use-package circadian
  :ensure t
  :config
  (setq calendar-latitude 59.3)
  (setq calendar-longitude 18.1)
  (setq circadian-themes '((:sunrise . doom-flatwhite)
                           (:sunset  . doom-plain-dark)))
  (circadian-setup))

(use-package fill-column-indicator
  :ensure t
  :config
  (define-globalized-minor-mode my-global-fci-mode fci-mode turn-on-fci-mode)
  (my-global-fci-mode 1)
  (setq fci-rule-column 90)) ; set page guide at 90 like BBEdit default

(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :config
  (progn
    (require 'smartparens-config)
    (smartparens-global-mode 1)
    (show-paren-mode t)))

(use-package auto-complete
  :ensure t
  :config
  (global-auto-complete-mode t))

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

(use-package magit
  :bind (("C-M-g" . magit-status)))

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

;; Investigate how to change map leader to <Space> like in vim...
;; (use-package evil
;;   :init
;;   (setq evil-want-integration t)
;;   (setq evil-want-keybinding nil)
;;   :config
;;   (evil-mode 1)
;;   (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state))

;;   ;; Use visual line motions even outside of visual-line-mode buffers
;;   (evil-global-set-key 'motion "j" 'evil-next-visual-line)
;;   (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

;;   (evil-set-initial-state 'messages-buffer-mode 'normal)
;;   (evil-set-initial-state 'dashboard-mode 'normal)

(use-package hydra)

(defhydra hydra-zoom (global-map "<f2>")
  "zoom"
  ("g" text-scale-increase "in")
  ("l" text-scale-decrease "out"))

(use-package faust-mode
  :ensure t
  :mode ("\\.dsp?\\'" . faust-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("ced4e3d3440ba3a74bbb2b107ba9e973373b5c656dcfd37c1ac7298cd974daf0" default))
 '(package-selected-packages
   '(circadian auto-complete faust-mode faust-lang evil markdown-mode fill-column-indicator which-key magit use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-line ((t (:inherit highlight :extend t :underline nil)))))
