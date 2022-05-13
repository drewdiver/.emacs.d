;; ENV SETUP
(setq inhibit-startup-message t)

(setq user-full-name "Drew Diver"
      user-mail-address "shout@drewdiver.com")

(menu-bar-mode -1)
(scroll-bar-mode -1) ; disable visible scrollbar
(tool-bar-mode -1) ; disable the toolbar
(tooltip-mode -1) ; disable tooltips
(set-fringe-mode -1) ; extra room
(setq visible-bell 1) ; silence
(blink-cursor-mode -1) ; are you blind?
(fset 'yes-or-no-p 'y-or-n-p) ; lazy
(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq-default tab-width 4
              indent-tabs-mode nil) ; tabs are spaces
(setq prelude-use-smooth-scrolling t)
(setq gc-cons-threshold 50000000)
(setq large-file-warning-threshold 100000000)

;; For specifics, use C-h v then type temporary-file-directory and hit enter.
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; USE-PACKAGE SETUP
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

;; CUSTOM FUNCTIONS
(defun er-switch-to-previous-buffer ()
  "Switch to previously open buffer."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun dd/send-buffer-to-jrnl ()
  "Sends the content of the current buffer to jrnl."
  (interactive)
  (call-process-region (point-min) (point-max) "jrnl")
  (message "Saved buffer contents in journal"))

;; CUSTOM KEYBINDINGS
(global-set-key (kbd "C-c E") 'eval-buffer) ; M-x eval-buffer becomes cumbersom
(global-set-key (kbd "C-c I") (lambda () (interactive) (find-file user-init-file))) ; open my init.el
(global-set-key (kbd "<escape>") 'keyboard-escape-quit) ; use esc to quit prompts
(global-set-key (kbd "C-x k") 'kill-this-buffer) ; kill current buffer
(global-set-key (kbd "C-c t") 'toggle-truncate-lines) ; turn off line wrapping
(global-set-key (kbd "C-c C-v") 'view-mode) ; enable view-mode
(global-set-key (kbd "C-c L") 'hl-line-mode) ; enable line highlight
(global-set-key (kbd "C-c l") 'linum-mode) ; enable line numbers for current buffer
(global-set-key (kbd "C-c b") #'er-switch-to-previous-buffer)
(global-set-key (kbd "C-c m") 'mu4e)

;; CUSTOM HOOKS
(add-hook 'text-mode-hook 'turn-on-auto-fill) ; automatic line-breaks for txt and markdown
(add-hook 'markdown-mode 'turn-on-auto-fill)

;; ERC SETUP
(setq erc-server "irc.libera.chat"
      erc-port 6697
      erc-nick "tiredsince1985")

;; MU4E EMAIL SETUP
;; Possible to add an "if mail-utils" found?
(require 'mu4e)
(setq
 mue4e-headers-skip-duplicates t
 mu4e-view-show-images t
 mu4e-view-show-addresses t
 mu4e-compose-format-flowed nil
 mu4e-date-format "%Y/%m/%d"
 mu4e-headers-date-format "%Y/%m/%d"
 mu4e-change-filenames-when-moving t
 mu4e-attachments-dir "~/Downloads"

 mu4e-maildir "~/Maildir"
 mu4e-refile-folder "/Archive"
 mu4e-sent-folder "/Sent"
 mu4e-drafts-folder "/Drafts"
 mu4e-trash-folder "/Trash")

(setq mu4e-get-mail-command "mbsync -a")

(setq
   message-send-mail-function   'smtpmail-send-it
   smtpmail-default-smtp-server "smtp.fastmail.com"
   smtpmail-smtp-server         "smtp.fastmail.com")

;; PACKAGES
(use-package doom-themes ;; flatwhite, plain, tokyo-night are all nice
  :ensure t
  :defer
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;; (load-theme 'doom-plain-dark t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config))

;; Must to run M-x all-the-icons-install-fonts
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(use-package circadian
  :ensure t
  :config
  (setq calendar-latitude 59.3)
  (setq calendar-longitude 18.1)
  (setq circadian-themes '((:sunrise . doom-flatwhite)
                           (:sunset  . doom-tokyo-night)))
  (circadian-setup))

(use-package fill-column-indicator
  :ensure t
  :config
  (define-globalized-minor-mode my-global-fci-mode fci-mode turn-on-fci-mode)
  ;; (my-global-fci-mode 1)
  (setq fci-rule-column 90) ; set page guide at 90 like BBEdit default
  (global-set-key [f7] 'fci-mode))

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

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

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

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package neotree
  :ensure t
  :config
  ;; Disable line-numbers minor mode for neotree
  (add-hook 'neo-after-create-hook (lambda (&optional dummy) (display-line-numbers-mode -1)))
  (global-set-key [f8] 'neotree-toggle))

(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :config
  ;; Recommended keymap prefix on macOS
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  ;; Recommended keymap prefix on Windows/Linux
  ;; (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  )

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

(use-package hydra)

(defhydra hydra-zoom (global-map "<f2>")
  "zoom"
  ("g" text-scale-increase "in")
  ("l" text-scale-decrease "out"))

(use-package faust-mode
  :ensure t
  :mode ("\\.dsp?\\'" . faust-mode))

(use-package go-mode
  :ensure t
  :mode ("\\.go?\\'" . go-mode))

;; Fix eshell etc for macOS
(use-package exec-path-from-shell
  :ensure t
  :init
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package howm
     :ensure t
     :config
     ;; Directory configuration
     (setq howm-home-directory "~/Sync/howm/")
     (setq howm-directory "~/Sync/howm/")
     (setq howm-keyword-file (expand-file-name ".howm-keys" howm-home-directory))
     (setq howm-history-file (expand-file-name ".howm-history" howm-home-directory))
     (setq howm-file-name-format "%Y/%m/%Y-%m-%d-%H%M%S.md"))

(use-package elfeed
  :ensure t
  :config
  (global-set-key (kbd "C-c w") 'elfeed)
  (global-set-key (kbd "C-c r") 'elfeed-update)
  (setq-default elfeed-search-filter "@1-days-ago")
  (setq elfeed-feeds
      '("http://nullprogram.com/feed/"
        "https://planet.emacslife.com/atom.xml"
        "https://leancrew.com/all-this/feed.json"
        "https://www.audiothing.net/feed/"
        "https://branch.climateaction.tech/feed/"
        "https://blog.codinghorror.com/rss/"
        "https://daringfireball.net/feeds/json"
        "https://www.dragonflydigest.com/feed"
        "http://morrick.me/archives/tag/english/feed"
        "https://flak.tedunangst.com/rss"
        "https://cdn.jwz.org/blog/feed/"
        "https://lwn.net/headlines/newrss"
        "https://mjtsai.com/blog/feed/"
        "https://mullvad.net/blog/feed/rss/"
        "https://nnw.ranchero.com/feed.json"
        "https://ubuntustudio.org/feed/"
        "https://onefoottsunami.com/feed/json/"
        "https://www.smbc-comics.com/comic/rss"
        ("https://lapcatsoftware.com/articles/atom.xml" apple blog)
        "https://torrentfreak.com/feed/"
        ("https://valhalladsp.com/feed/" DSP blog)
        "Http://nullprogram.com/feed/"
        "https://planet.emacslife.com/atom.xml"
        "https://www.omgubuntu.co.uk/feed"
        "https://nitter.net/viznut/rss"
        "https://nitter.net/2600stockholm/rss")))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("ced4e3d3440ba3a74bbb2b107ba9e973373b5c656dcfd37c1ac7298cd974daf0" default))
 '(package-selected-packages
   '(elfeed go-mode exec-path-from-shell neotree projectile helpful counsel ivy-rich helm circadian auto-complete faust-mode faust-lang evil markdown-mode fill-column-indicator which-key magit use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-line ((t (:inherit highlight :extend t :underline nil)))))
