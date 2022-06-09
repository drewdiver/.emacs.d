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
(set-default 'truncate-lines t) ;; disable line wrapping

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

;; could call delete-active-region
;; open a mini scratch buffer named jrnl?
(defun dd/send-buffer-to-jrnl ()
  "Sends the content of the current buffer to jrnl."
  (interactive)
  (call-process-region (point-min) (point-max) "jrnl")
  (message "Saved buffer contents in journal"))

;; CUSTOM KEYBINDINGS
(global-set-key (kbd "C-x r;") 'comment-region)
(global-set-key (kbd "C-c E") 'eval-buffer) ; M-x eval-buffer becomes cumbersom
(global-set-key (kbd "C-c I") (lambda () (interactive) (find-file user-init-file))) ; open my init.el
(global-set-key (kbd "<escape>") 'keyboard-escape-quit) ; use esc to quit prompts
(global-set-key (kbd "C-x k") 'kill-this-buffer) ; kill current buffer
(global-set-key (kbd "C-c T") 'toggle-truncate-lines) ; turn off line wrapping
(global-set-key (kbd "C-c C-v") 'view-mode) ; enable view-mode
(global-set-key (kbd "C-c L") 'hl-line-mode) ; enable line highlight
(global-set-key (kbd "C-c l") 'linum-mode) ; enable line numbers for current buffer
(global-set-key (kbd "C-c b") #'er-switch-to-previous-buffer)
(global-set-key (kbd "C-c m") 'mu4e)
(global-set-key (kbd "C-c e") 'elfeed)
(global-set-key (kbd "C-c u") 'elfeed-update)
(global-set-key [f7] 'fci-mode)
(global-set-key (kbd "C-c t") 'todotxt)

;; Enable line wrapping for some modes
(dolist (mode '(text-mode-hook
                markdown-mode-hook))
  (add-hook mode (lambda () (auto-fill-mode 1))))

;; Enable line numbers for some modes
(dolist (mode '(python-mode-hook
                sh-mode-hook
                emacs-lisp-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))

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
  (setq circadian-themes '((:sunrise . doom-flatwhite) ;; tango light is also nice...
                           (:sunset  . doom-zenburn)))
  (circadian-setup))

(use-package fill-column-indicator
  :defer t
  :config
  (define-globalized-minor-mode my-global-fci-mode fci-mode turn-on-fci-mode)
  ;; (my-global-fci-mode 1)
  (setq fci-rule-column 90)) ; set page guide at 90 like BBEdit default)

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

(use-package company
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-idle-delay 0) ;; no delay in showing suggestions
  (setq company-minimum-prefix-length 1) ;; show suggestion after 1 chatacter
  (setq company-selection-wrap-around t) ;; auto wrap at end of list
  (company-tng-configure-default)) ;; use tab to cycle

(use-package good-scroll
  :ensure t
  :config
  (good-scroll-mode 1)
  (global-set-key [next] #'good-scroll-up-full-screen)
  (global-set-key [prior] #'good-scroll-down-full-screen))

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

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

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

(use-package markdown-mode
  :mode ("\\\.md\\'" . markdown-mode)
  :hook
  (markdown-mode . auto-fill-mode))

(use-package faust-mode :mode ("\\.dsp?\\'" . faust-mode))

;; Fix eshell etc for macOS
(use-package exec-path-from-shell
  :init
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

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

(use-package elfeed
  :defer t
  :config
  (setq-default elfeed-search-filter "@2-weeks-ago +unread ")

  ;; Entries older than 2 weeks are marked as read
  (add-hook 'elfeed-new-entry-hook
          (elfeed-make-tagger :before "2 weeks ago"
                              :remove 'unread))

  ;; (defun add-entry-categories-to-tags (entry)
  ;;   (dolist (category (elfeed-meta entry :categories) entry)
  ;;     (let ((tag (tagize-for-elfeed category)))
  ;;       (when tag
  ;;         (elfeed-tag entry tag)))))
  ;; (add-hook 'elfeed-new-entry-hook #'add-entry-categories-to-tags)

  ;; ;; useful if I want to mute uninteresting stuff
  ;; (add-hook 'elfeed-new-entry-hook
  ;;           (elfeed-make-tagger :feed-url "example\\.com"
  ;;                               :entry-title '(not "something interesting")
  ;;                               :add 'junk
  ;;                               :remove 'unread))
  
  ;; auto-tagging
  (add-hook 'elfeed-new-entry-hook
            (elfeed-make-tagger
             :feed-url "dragonflydigest\\.com"
             :entry-title "^Lazy Reading"
             :add 'lazy-reading))
  
  (add-hook 'elfeed-new-entry-hook
            (elfeed-make-tagger
             :feed-url "dragonflydigest\\.com"
             :entry-title "^In Other BSDs"
             :add 'in-other-bsds))

  (defface elfeed-lazy-reading
    '((t :background "#e2e9c1"))
    "Highlights the Lazy Reading entry."
    :group 'elfeed)

  (push '(lazy-reading elfeed-lazy-reading)
        elfeed-search-face-alist)
           
  (defface elfeed-in-other-bsds   
    '((t :background "#e2e9c1"))
    "Highlights the In Other BSDs entry."
    :group 'elfeed)

  (push '(in-other-bsds elfeed-in-other-bsds)
        elfeed-search-face-alist)
  
  (setq elfeed-feeds
        '(("http://nullprogram.com/feed/" blog emacs dev)
          ("https://theoverspill.blog/feed/" blog news tech)
          ("https://planet.emacslife.com/atom.xml" emacs)
          ("https://leancrew.com/all-this/feed/" blog dev apple)
          ("https://www.audiothing.net/feed/" audio dsp)
          ("https://blog.codinghorror.com/rss/" blog)
          ("https://daringfireball.net/feeds/main" blog news apple)
          ("https://www.dragonflydigest.com/feed" blog bsd)
          ("https://scriptingosx.com/feed/" blog apple devops)
          ("http://morrick.me/archives/tag/english/feed" blog apple)
          ("https://flak.tedunangst.com/rss" blog bsd dev)
          ("https://cdn.jwz.org/blog/feed/" blog tech)
          ("https://lwn.net/headlines/newrss" news linux)
          ("https://mjtsai.com/blog/feed/" blog apple dev)
          ("https://mullvad.net/blog/feed/rss/" blog security)
          ("https://ubuntustudio.org/feed/" linux audio)
          ("https://onefoottsunami.com/feed/atom/" blog news)
          ("https://www.smbc-comics.com/comic/rss" comics)
          ("https://lapcatsoftware.com/articles/atom.xml" blog apple)
          ("https://torrentfreak.com/feed/" news piracy)
          ("https://valhalladsp.com/feed/" blog audio dsp)
          ("https://planet.emacslife.com/atom.xml" emacs)
          ("https://www.omgubuntu.co.uk/feed" linux)
          ("https://nitter.net/viznut/rss" permacomputing twitter)
          ("https://nitter.net/2600stockholm/rss" twitter)
          ("https://web3isgoinggreat.com/feed.xml" news crypto)
          ("https://fastmail.blog/rss/" blog)
          ("https://www.theatlantic.com/feed/author/david-frum/" news world)
          ("https://www.theatlantic.com/feed/author/jonathan-haidt/" news world)
          ("https://www.theregister.com/headlines.atom" news tech)
          ("https://feeds2.feedburner.com/typepad/krisdedecker/lowtechmagazineenglish" permacomputing)
          ("https://www.commitstrip.com/en/feed/" comics))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("353ffc8e6b53a91ac87b7e86bebc6796877a0b76ddfc15793e4d7880976132ae" "ced4e3d3440ba3a74bbb2b107ba9e973373b5c656dcfd37c1ac7298cd974daf0" default))
 '(exwm-floating-border-color "#252630")
 '(fci-rule-color "#9099c0")
 '(highlight-tail-colors ((("#222e36") . 0) (("#29313b") . 20)))
 '(jdee-db-active-breakpoint-face-colors (cons "#414868" "#b4f9f8"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#414868" "#73daca"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#414868" "#8189af"))
 '(objed-cursor-color "#f7768e")
 '(package-selected-packages
   '(good-scroll good-scroll-mode todotxt all-the-icons olivetti emacsql emacs-request wallabag focus elpy elfeed-web elfeed go-mode exec-path-from-shell neotree projectile helpful counsel ivy-rich helm circadian auto-complete faust-mode faust-lang evil markdown-mode fill-column-indicator which-key magit use-package))
 '(pdf-view-midnight-colors (cons "#a9b1d6" "#1a1b26"))
 '(rustic-ansi-faces
   ["#1a1b26" "#f7768e" "#73daca" "#e0af68" "#7aa2f7" "#bb9af7" "#b4f9f8" "#a9b1d6"])
 '(vc-annotate-background "#1a1b26")
 '(vc-annotate-color-map
   (list
    (cons 20 "#73daca")
    (cons 40 "#97cba9")
    (cons 60 "#bbbd88")
    (cons 80 "#e0af68")
    (cons 100 "#eaa966")
    (cons 120 "#f4a365")
    (cons 140 "#ff9e64")
    (cons 160 "#e89c94")
    (cons 180 "#d19bc5")
    (cons 200 "#bb9af7")
    (cons 220 "#cf8ed4")
    (cons 240 "#e382b1")
    (cons 260 "#f7768e")
    (cons 280 "#d97a96")
    (cons 300 "#bc7f9e")
    (cons 320 "#9e84a6")
    (cons 340 "#9099c0")
    (cons 360 "#9099c0")))
 '(vc-annotate-very-old-color nil))
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

;
;(if (not (buffer-exists "jrnl"))
;    (generate-new-buffer "jrnl"))

(add-hook 'after-init-hook 'howm-menu)
