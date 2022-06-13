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
(set-default 'truncate-lines t) ;; disable line wrapping

;; For specifics, use C-h v then type temporary-file-directory and hit enter.
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Enable line wrapping for some modes
(dolist (mode '(text-mode-hook
                markdown-mode-hook))
  (add-hook mode (lambda () (auto-fill-mode 1))))

;; Enable line numbers for some modes
(dolist (mode '(python-mode-hook
                sh-mode-hook
                emacs-lisp-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))

;; Use mu4e on Linux but skip mu4e setup on macOS
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

(defun er-switch-to-previous-buffer ()
  "Switch to previously open buffer."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CUSTOM KEYBINDINGS                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-c i") (lambda () (interactive) (find-file user-init-file)))
(global-set-key (kbd "C-c e") 'eval-buffer)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit) ; use esc to quit prompts
(global-set-key (kbd "C-x k") 'kill-this-buffer) ; kill current buffer
(global-set-key (kbd "C-c T") 'toggle-truncate-lines) ; turn off line wrapping
(global-set-key (kbd "C-c C-v") 'view-mode) ; enable view-mode
(global-set-key (kbd "C-c L") 'hl-line-mode) ; enable line highlight
(global-set-key (kbd "C-c l") 'linum-mode) ; enable line numbers for current buffer
(global-set-key (kbd "C-c b") #'er-switch-to-previous-buffer)
(global-set-key (kbd "C-c m") 'mu4e)
(global-set-key (kbd "C-c t") 'todotxt)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; USE-PACKAGE SETUP AND PACKAGES BELOW ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (doom-themes-visual-bell-config))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(use-package circadian
  :config
  (setq calendar-latitude 59.3)
  (setq calendar-longitude 18.1)
  (setq circadian-themes '((:sunrise . doom-opera-light) ;; color-theme-standard
                           (:sunset  . doom-zenburn)))
  (circadian-setup))

(use-package exec-path-from-shell
  :init
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; macOS only packages can go within here
(if (eq system-type 'darwin)
    (use-package reveal-in-osx-finder
      :ensure t
      :bind
      ("C-c z" . reveal-in-osx-finder)))

(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :config
  (progn
    (require 'smartparens-config)
    (smartparens-global-mode 1)
    (show-paren-mode t)))

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(use-package company
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-idle-delay 0) ;; no delay in showing suggestions
  (setq company-minimum-prefix-length 1) ;; show suggestion after 1 chatacter
  (setq company-selection-wrap-around t) ;; auto wrap at end of list
  (company-tng-configure-default)) ;; use tab to cycle

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

;; Nicer buffer search
(use-package swiper :ensure t)

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

(use-package ivy-rich :init (ivy-rich-mode 1))

(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :config
  (if (eq system-type 'darwin)
      (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map) ;; Recommended keymap prefix on macOS
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)) ;; Recommended keymap prefix on Windows/Linux
  )

(use-package elpy
  :ensure t
  :init
  (elpy-enable)
  :config
  (setq elpy-rpc-python-command "python3")
  (setq python-shell-interpreter "python3"))

(use-package markdown-mode
  :mode ("\\\.md\\'" . markdown-mode)
  :hook
  (markdown-mode . auto-fill-mode))

(use-package todotxt
  :ensure t
  :config
  (setq todotxt-file "~/journal/todo.txt"))

(use-package howm
     :config
     ;; Directory configuration
     (setq howm-home-directory "~/howm/")
     (setq howm-directory "~/howm/")
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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(reveal-in-osx-finder all-the-icons elpy company projectile markdown-mode howm todotxt exec-path-from-shell ivy-rich helpful counsel rainbow-delimiters which-key smartparens circadian doom-themes use-package doom-modeline)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Measure startup time
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))
