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
(setq linum-format "%d ") ;; add space after line number

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
(global-set-key (kbd "C-c e") 'elfeed)
(global-set-key (kbd "C-c u") 'elfeed-update)

;; CUSTOM HOOKS
;; (add-hook 'text-mode-hook 'turn-on-auto-fill) ; automatic line-breaks for txt and markdown
;; (add-hook 'markdown-mode 'turn-on-auto-fill)

;; ;; Disable line numbers for some modes
;; (dolist (mode '(markdown-mode-hook
;;                 term-mode-hook
;;                 eshell-mode-hook))
;;   (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; ERC SETUP
(setq erc-server "irc.libera.chat"
      erc-port 6697
      erc-nick "tiredsince1985")

;; skip mu4e on macOS
(if (not (eq system-type 'darwin))
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
)

;; PACKAGES
(use-package doom-themes ;; flatwhite, plain, tokyo-night are all nice
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;; (load-theme 'doom-plain-dark t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config))

;; Must to run M-x all-the-icons-install-fonts
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(use-package circadian
  :config
  (setq calendar-latitude 59.3)
  (setq calendar-longitude 18.1)
  (setq circadian-themes '((:sunrise . tango) ;; 
                           (:sunset  . doom-tokyo-night)))
  (circadian-setup))

(use-package fill-column-indicator
  :defer t
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

(use-package elpy
  :ensure t
  :init
  (elpy-enable)
  :config
  (setq elpy-rpc-python-command "python3")
  (setq python-shell-interpreter "python3"))

;; (use-package company
;;   :ensure t
;;   :init
;;   (add-hook 'after-init-hook 'global-company-mode)
;;    (eval-after-load "company"
;;     '(add-to-list 'company-backends '(company-anaconda :with company-capf))))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

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

(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :config
  (if (eq system-type 'darwin)
      (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map) ;; Recommended keymap prefix on macOS
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)) ;; Recommended keymap prefix on Windows/Linux
  )

(use-package markdown-mode
  :mode ("\\\.md\\'" . markdown-mode)
  :hook (markdown-mode . auto-fill-mode))

(use-package faust-mode
  :mode ("\\.dsp?\\'" . faust-mode))

;; Fix eshell etc for macOS
(use-package exec-path-from-shell
  :init
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package howm
     :config
     ;; Directory configuration
     (setq howm-home-directory "~/Sync/howm/")
     (setq howm-directory "~/Sync/howm/")
     (setq howm-keyword-file (expand-file-name ".howm-keys" howm-home-directory))
     (setq howm-history-file (expand-file-name ".howm-history" howm-home-directory))
     (setq howm-file-name-format "%Y/%m/%Y-%m-%d-%H%M%S.md")
     ;; Rename buffers to their title
     (add-hook 'howm-mode-hook 'howm-mode-set-buffer-name)
     (add-hook 'after-save-hook 'howm-mode-set-buffer-name)
     ;; Default recent to sorting by mtime
     (advice-add 'howm-list-recent :after #'howm-view-sort-by-mtime)
     ;; Default all to sorting by creation, newest first
     (advice-add 'howm-list-all :after #'(lambda () (howm-view-sort-by-date t)))
     (define-key howm-menu-mode-map "\C-h" nil)
     (define-key riffle-summary-mode-map "\C-h" nil)
     (define-key howm-view-contents-mode-map "\C-h" nil)
     ;; Experiment with ripgrep
     (setq howm-view-use-grep t)
     (setq howm-view-grep-command "rg")
     (setq howm-view-grep-option "-nH --no-heading --color never")
     (setq howm-view-grep-extended-option nil)
     (setq howm-view-grep-fixed-option "-F")
     (setq howm-view-grep-expr-option nil)
     (setq howm-view-grep-file-stdin-option nil)

     ;; counsel-rg for howm
     (defun howm-list--counsel-rg (match)
       (if (string= match "")
       (howm-list-all)
     (if (or (null ivy--old-cands)
         (equal ivy--old-cands '("No matches found")))
             (message "No match")
       (let ((howm-view-use-grep
          #'(lambda (str file-list &optional fixed-p force-case-fold)
                      (mapcar
                       (lambda (cand)
             (if (string-match "\\`\\(.*\\):\\([0-9]+\\):\\(.*\\)\\'" cand)
                             (let ((file (match-string-no-properties 1 cand))
                   (line (match-string-no-properties 2 cand))
                   (match-line (match-string-no-properties 3 cand)))
                               (list (expand-file-name file howm-directory)
                                     (string-to-number line)
                                     match-line))))
                       ivy--old-cands))))
             (howm-search ivy--old-re t)
             (riffle-set-place
          (1+ (cl-position match ivy--old-cands :test 'string=)))))))

      (defun howm-counsel-rg ()
        "Interactively grep for a string in your howm notes using rg."
        (interactive)
        (let ((default-directory howm-directory)
              (counsel-ag-base-command counsel-rg-base-command)
              (counsel-ag-command (counsel--format-ag-command "--glob=!*~" "%s")))
          (ivy-read "Search all (rg): "
                #'counsel-ag-function
                :dynamic-collection t
                :keymap counsel-ag-map
                :action #'howm-list--counsel-rg
                :require-match t
                :caller 'counsel-rg)))

      (define-key global-map (concat howm-prefix "r") 'howm-counsel-rg))

(use-package elfeed
  :defer t
  :config
  (setq-default elfeed-search-filter "@1-week-ago +unread ")
  (setq elfeed-feeds
      '("http://nullprogram.com/feed/"
        "https://planet.emacslife.com/atom.xml"
        "https://leancrew.com/all-this/feed/"
        "https://www.audiothing.net/feed/"
        "https://branch.climateaction.tech/feed/"
        "https://blog.codinghorror.com/rss/"
        "https://daringfireball.net/feeds/main"
        "https://www.dragonflydigest.com/feed"
        "http://morrick.me/archives/tag/english/feed"
        "https://flak.tedunangst.com/rss"
        "https://cdn.jwz.org/blog/feed/"
        "https://lwn.net/headlines/newrss"
        "https://mjtsai.com/blog/feed/"
        "https://mullvad.net/blog/feed/rss/"
        "https://ubuntustudio.org/feed/"
        "https://onefoottsunami.com/feed/atom/"
        "https://www.smbc-comics.com/comic/rss"
        ("https://lapcatsoftware.com/articles/atom.xml" apple blog)
        "https://torrentfreak.com/feed/"
        ("https://valhalladsp.com/feed/" DSP blog)
        "Http://nullprogram.com/feed/"
        "https://planet.emacslife.com/atom.xml"
        "https://www.omgubuntu.co.uk/feed"
        "https://nitter.net/viznut/rss"
        "https://nitter.net/2600stockholm/rss"
        "https://web3isgoinggreat.com/feed.xml")))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("353ffc8e6b53a91ac87b7e86bebc6796877a0b76ddfc15793e4d7880976132ae" "ced4e3d3440ba3a74bbb2b107ba9e973373b5c656dcfd37c1ac7298cd974daf0" default))
 '(package-selected-packages
   '(focus elpy elfeed-web elfeed go-mode exec-path-from-shell neotree projectile helpful counsel ivy-rich helm circadian auto-complete faust-mode faust-lang evil markdown-mode fill-column-indicator which-key magit use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-line ((t (:inherit highlight :extend t :underline nil)))))

;; Measure startup time
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))
