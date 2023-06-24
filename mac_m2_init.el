;; (when (fboundp 'native-compile-async)
;;   (native-compile-async "/Users/mehmetaktas/.emacs.d/init.el"))

;; (defvar comp-deferred-compilation-deny-list ())

(setq straight-disable-native-compile t)

;; cd ~/.emacs.d/; git clone https://github.com/radian-software/straight.el.git
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


;; -------------------------------------  Package setup  ------------------------------------- ;;
;; Ref: https://stackoverflow.com/questions/67688140/emacs-definition-is-void-use-package
(require 'package)                   ; Bring in to the environment all package management functions

;; A list of package repositories
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org"   . "https://orgmode.org/elpa/")
			 ("elpa"  . "https://elpa.gnu.org/packages/")))

(package-initialize)                 ; Initializes the package system and prepares it to be used

(unless package-archive-contents     ; Unless a package archive already exists,
  (package-refresh-contents))        ; Refresh package contents so that Emacs knows which packages to load


;; Initialize use-package on non-linux platforms
(unless (package-installed-p 'use-package)        ; Unless "use-package" is installed, install "use-package"
  (package-install 'use-package))

(require 'use-package)                            ; Once it's installed, we load it using require

;; Make sure packages are downloaded and installed before they are run
;; also frees you from having to put :ensure t after installing EVERY PACKAGE.
(setq use-package-always-ensure t)

;; -------------------------------------------------------------------------------------
;; ----------------------------    THEME     -------------------------------------------
;; -------------------------------------------------------------------------------------
;; (load-theme 'wombat)
;; (load-theme 'spacemacs-light t)
(load-theme 'spacemacs-dark t)
;; (load-theme 'zenburn t)
;; (load-theme 'twilight-bright)
;; (load-theme 'autumn-light)
;; (load-theme 'paper)
;; (load-theme 'occidental)

;; (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
;; (require 'cl-lib)
;; (load-file "~/.emacs.d/themes/spacemacs-common.el")
;; (load-file "~/.emacs.d/themes/spacemacs-light-theme.el")
;; (load-file "~/.emacs.d/themes/aalto-light-theme.el")

;; (setq base16-theme-256-color-source "base16-shell")

;; a great font: https://www.fontyukle.net/en/Monaco.ttf
(condition-case nil
    (set-default-font "Monaco")
  (error nil))

;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(default ((t (:inherit nil :stipple nil :background "#242424" :foreground "#f6f3e8" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 113 :width normal :foundry "APPL" :family "Monaco"))))
;;  '(fringe ((t (:background "#242424"))))
;;  '(linum ((t (:inherit (shadow default) :background "#191919" :foreground "#505050")))))

;; -------------------------------------------------------------------------------------
;; ---------------------    ENHANCEMENTS     -------------------------------------------
;; -------------------------------------------------------------------------------------
;; Add a newline at the end of file
(setq require-final-newline t)

;; Make mouse work
(require 'mwheel)
(require 'mouse)
(xterm-mouse-mode t)
(mouse-wheel-mode t)
(global-set-key [mouse-4] 'previous-line)
(global-set-key [mouse-5] 'next-line)


;; remove unecessary UI
(menu-bar-mode -1)
;; (scroll-bar-mode 0)
;; (tool-bar-mode 0)
(setq visible-bell 1)
(setq ring-bell-function 'ignore)

;; startup screen
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; remove whitespace before saving
(add-hook 'before-save-hook 'whitespace-cleanup)

;; faster to quit
(fset 'yes-or-no-p 'y-or-n-p)

;; highlight selected text
(transient-mark-mode t)

;; set path
(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
(add-to-list 'exec-path "/usr/local/bin")

;; display matching parenthesis
(show-paren-mode 1)

;; refresh buffers when any file change
(global-auto-revert-mode t)

;; track recently opened file
(recentf-mode t)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)
(setq recentf-max-saved-items 1000)
(setq recentf-max-menu-items 50)

;; display pictures and other compressed files
(setq auto-image-file-mode t)
(setq auto-compression-mode t)

;; line and column numbering
(column-number-mode 1)
(line-number-mode 1)

;; code folding
(global-set-key (kbd "C-c C-d") 'hs-hide-all)
(global-set-key (kbd "C-c C-f") 'hs-show-all)
(global-set-key (kbd "C-c C-c") 'hs-toggle-hiding)
(add-hook 'prog-mode-hook #'(lambda () (hs-minor-mode t) ))

;; Search using regexp
(global-set-key "\C-s" 'isearch-forward-regexp)
(global-set-key "\C-r" 'isearch-backward-regexp)

;; Usable on OSX and windows
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'alt)
(setq mac-right-option-modifier 'super)
(setq w32-get-true-file-attributes nil)
(setq vc-handled-backends nil)
(remove-hook 'find-file-hooks 'vc-find-file-hook)

;; scroll
(setq scroll-step 1)
(setq scroll-margin 20)
(setq scroll-conservatively 1)

;; disable backup files
(setq make-backup-files nil)

;; auto wraping
(set-default 'truncate-lines t)
(add-hook 'text-mode-hook (lambda () (setq truncate-lines nil)))

;; line wrap
(setq linum-format "%d ")
;;(global-linum-mode 1)

;; usefull shortcuts
(global-set-key [f3] 'comment-region)
(global-set-key [f4] 'uncomment-region)
(global-set-key [f5] 'eshell)
(global-set-key [f11] 'toggle-frame-fullscreen)
(global-set-key (kbd "C-h C-s") 'info-apropos)

;; ARTIST MODE
(eval-after-load "artist" '(define-key artist-mode-map [(down-mouse-3)] 'artist-mouse-choose-operation))

;; EWW - emacs web browser
(setq eww-search-prefix "https://www.google.com.au/search?q=")
(setq shr-color-visible-luminance-min 78) ;; improve readability (especially for google search)
(setq url-user-agent "User-Agent: Mozilla/5.0 (iPhone; U; CPU iPhone OS 4_0 like Mac OS X; en-us) AppleWebKit/532.9 (KHTML, like Gecko) Version/4.0.5 Mobile/8A293 Safari/6531.22.7\n")
(global-set-key (kbd "C-c b") 'eww)

;; Setup the package manager
(defun load-package-manager ()
  (package-initialize)
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/")))
(add-hook 'after-init-hook 'load-package-manager)

;; Window split line
(set-face-attribute 'vertical-border nil :foreground "#303030")

;; CAUTION: This turns spaces into tabs after saving
;; Remove all trailing white spaces while saving
;; (add-hook 'write-file-hooks 'delete-trailing-whitespace)

;; -------------------------------------------------------------------------------------
;; ---------------------      PLUGINS          -----------------------------------------
;; -------------------------------------------------------------------------------------

;; Our plugins configuration will leave under the conf folder, so let's load this up:
;; (mapc
;;  (lambda(path) (load-file path))
;;  (condition-case nil
;;      (directory-files "~/.emacs.d/conf/" t "\.el$")
;;    (erro
;;    r (make-directory "~/.emacs.d/conf/"))))

;; ---------------------  Mehmet added  -------------------- ;;

;; load emacs 24's package system. Add MELPA repository.
;; (when (>= emacs-major-version 24)
;;   (require 'package)
;;   (add-to-list
;;    'package-archives
;;    ;; '("melpa" . "http://stable.melpa.org/packages/") ; many packages won't show if using stable
;;    '("melpa" . "http://melpa.milkbox.net/packages/")
;;    t))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("1a1266e25ed97448bbe80f246f53372d4b914d30802711abfda7afcbf2f7b3ec" "859ff6182156e4bea960c2c7678f8b3da23961046b855e805f0f5a5d09b92658" "aa6638f0cd2ba2c68be03220ea73495116dc6f0b625405ede34087c1babb71ae" "76b4632612953d1a8976d983c4fdf5c3af92d216e2f87ce2b0726a1f37606158" "a7760b614d51ba59af9bd4a87014e688cdcb7df36e971e21abc23cc7ad0cdfbc" default))
 '(flycheck-flake8-maximum-line-length 200)
 '(flycheck-flake8rc "pyproject.toml")
 '(git-commit-summary-max-length 100)
 '(package-selected-packages
   '(undo-fu silkworm-theme smooth-scroll ## find-file-in-project helm-ag dumb-jump helm))
 '(warning-suppress-types '((magit))))

;; (load-theme 'silkworm)

;; (projectile-global-mode)
;; (setq projectile-enable-caching t)

;; (setq x-select-enable-clipboard t)
;; (setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

;; Tell emacs where is your personal elisp lib dir
(add-to-list 'load-path "~/.emacs.d/lisp/")
;; load the packaged named xyz.
(load "highlight-symbol") ;; best not to include the ending “.el” or “.elc”
;; (load "move-text")

;; (custom-set-variables
;;  '(highlight-symbol-colors
;;    (quote
;;     ("DeepPink" "cyan" "DarkRed" "DarkBlue" "tomato" "magenta1" )))
;;  )

(defun reload-dotemacs ()
  (interactive)
  (load-file "~/.emacs.d/init.el"))

(defun ssh-o1 ()
  (interactive)
  (find-file "/ssh:mehmet@console.sb1.orbit-lab.org:/home/mehmet"))

(defun ssh-o2 ()
  (interactive)
  (find-file "/ssh:mehmet@console.sb2.orbit-lab.org:/home/mehmet"))

(defun ssh-o4 ()
  (interactive)
  (find-file "/ssh:mehmet@console.sb4.orbit-lab.org:/home/mehmet"))

(defun ssh-o1-1 ()
  (interactive)
  (find-file "/ssh:mehmet@console.sb1.orbit-lab.org|ssh:root@node1-1:/root/edge-load-balance"))

(defun ssh-o4-1 ()
  (interactive)
  (find-file "/ssh:mehmet@console.sb4.orbit-lab.org|ssh:root@node1-1:/root/edge-load-balance"))

(defun ssh-o4-5 ()
  (interactive)
  (find-file "/ssh:mehmet@console.sb4.orbit-lab.org|ssh:root@node1-5:/root/edge-load-balance"))

(require 'tramp)
(setq tramp-default-method "sshx")
(setq tramp-local-host-regexp nil)

(add-to-list 'tramp-connection-properties
	     (list (regexp-quote "/sshx:user@host:")
		   "remote-shell" "/bin/bash"))

;; (setq tramp-verbose 8)
;; (add-to-list 'tramp-default-proxies-alist
;;              '("ORBIT" nil "/ssh:mehmet@console.sb1.orbit-lab.org:"))

;; (defun ssh-amarel ()
;;   (interactive)
;;   (find-file "/ssh:mehmet@console.sb1.orbit-lab.org|ssh:mfa51@amarel.hpc.rutgers.edu:/home/mfa51"))

;; Note: Notice `sshx`!
;; Ref:
;; - https://www.bounga.org/tips/2017/11/30/fix-emacs-tramp-lag-and-timeout/
;; - https://www.reddit.com/r/emacs/comments/uxqafc/has_tramp_ever_work_for_you_flawlessly/
(defun ssh-amarel ()
  (interactive)
  (find-file "/sshx:mfa51@amarel.hpc.rutgers.edu:/home/mfa51"))

;; https://emacs.stackexchange.com/questions/47424/tramp-gcloud-compute-ssh-not-working
(add-to-list 'tramp-methods
	     '("gcssh"
	      (tramp-login-program        "gcloud compute ssh")
	      (tramp-login-args           (("%h")))
	      (tramp-async-args           (("-q")))
	      (tramp-remote-shell         "/bin/sh")
	      (tramp-remote-shell-args    ("-c"))
	      (tramp-gw-args              (("-o" "GlobalKnownHostsFile=/dev/null")
					   ("-o" "UserKnownHostsFile=/dev/null")
					   ("-o" "StrictHostKeyChecking=no")))
	      (tramp-default-port         22)))

(defun ssh-mehmet-docker ()
 (interactive)
 (find-file "/gcssh:mehmet_overjet_ai@mehmet-docker:/home/mehmet_overjet_ai"))

;; (defun ssh-gcp ()
;;   (interactive)
;;   (find-file "/gcloud compute ssh:mehmet_overjet_ai@mehmet-1-vm:/home/mehmet_overjet_ai"))


;; (add-hook 'python-mode-hook
;;   (lambda ()
;;     (setq indent-tabs-mode t)
;;     (setq tab-width 2)
;;     (setq python-indent-offset 2)))

;; (add-hook 'shell-mode-hook
;;   (lambda ()
;;     (setq indent-tabs-mode t)
;;     (setq tab-width 2)
;;     (setq python-indent-offset 2)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; indentation
;; Note: setq-default does not work for some reason
(setq custom-tab-width 4)

(setq indent-tabs-mode nil)

;; (add-hook 'python-mode-hook
;;   (lambda ()
;;     (setq indent-tabs-mode nil)))
(setq python-indent-offset custom-tab-width)

(defun disable-tabs ()
  (interactive)
  (setq indent-tabs-mode nil))

(defun enable-tabs  ()
  (local-set-key (kbd "TAB") 'tab-to-tab-stop)
  (setq indent-tabs-mode t)
  (setq tab-width custom-tab-width))

;; Automatically enable anaconda-mode in all python buffers
;; (add-hook 'python-mode-hook 'anaconda-mode)

;; (package-initialize)
;; (elpy-enable)

;; Kills the entire line plus the newline whenever invoke kill-line (C-k)
(setq kill-whole-line t)


;; https://www.emacswiki.org/emacs/CopyWithoutSelection
(defun get-point (symbol &optional arg)
  "get the point"
  (funcall symbol arg)
  (point))

(defun copy-thing (begin-of-thing end-of-thing &optional arg)
  "Copy thing between beg & end into kill ring."
  (save-excursion
    (let ((beg (get-point begin-of-thing 1))
	  (end (get-point end-of-thing arg)))
      (copy-region-as-kill beg end))))

(defun paste-to-mark (&optional arg)
  "Paste things to mark, or to the prompt in shell-mode."
  (unless (eq arg 1)
    (if (string= "shell-mode" major-mode)
	(comint-next-prompt 25535)
      (goto-char (mark)))
    (yank)))

(defun copy-word (&optional arg)
  "Copy words at point into kill-ring"
  (superword-mode 1)
  (interactive "P")
  (copy-thing 'backward-word 'forward-word arg)
  ;;(paste-to-mark arg)
  (superword-mode -1)
  (message "Copied word"))

(defun select-word (&optional arg)
  (superword-mode 1)
  (interactive "P")
  (left-word)
  (mark-word)
  (superword-mode -1)
  (message "Selected word"))

;; https://stackoverflow.com/questions/22479533/emacs-copy-current-line
(defun copy-line ()
  (interactive)
  (save-excursion
    (back-to-indentation)
    (kill-ring-save
     (point)
     (line-end-position)))
     (message "Copied line"))

(defun projectile-ag-word (&optional arg)
  (interactive)
  (copy-word)
  ;; (projectile-ag)
  (message "projectile-ag-word"))

;; Note: Run `brew install ag`. Otherwise, `helm-ag` will not work.
(defun helm-ag-word-at-point (&optional arg)
  (interactive)
  (let ((helm-ag-insert-at-point 'symbol))
    (helm-do-ag-project-root))
  (message "helm-ag-word-at-point"))

;; -------------------------------------  jedi  ------------------------------------- ;;
;; Ref: http://tkf.github.io/emacs-jedi/latest/
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)                 ; optional

;; Ref: https://stackoverflow.com/questions/43550507/configure-jedi-not-to-auto-complete-automatically
(setq jedi:tooltip-method nil)


;; -------------------------------------  python-pytest  ------------------------------------- ;;
;; https://shahinism.com/en/posts/emacs-python-pytest/
(use-package python-pytest
  :after python
  :custom
  (python-pytest-arguments
   '("--color"          ;; colored output in the buffer
     "--failed-first"   ;; run the previous failed tests first
     "--pdb"
     "--maxfail=1"))    ;; exit in 5 continuous failures in a run
  ;; :config
  ;; (which-key-declare-prefixes-for-mode 'python-mode "SPC pt" "Testing")
  ;; (evil-leader/set-key-for-mode 'python-mode
  ;;   "ptp" 'python-pytest-popup
  ;;   "ptt" 'python-pytest
  ;;   ;; "ptf" 'python-pytest-file
  ;;   ;; "ptF" 'python-pytest-file-dwim
  ;;   "ptf" 'python-pytest-function
  ;;   "ptF" 'python-pytest-function-dwim
  ;;   "ptl" 'python-pytest-last-failed)
  )

(global-set-key (kbd "<ESC> ptd") 'python-pytest-dispatch)
(global-set-key (kbd "<ESC> ptm") 'python-pytest-file)
(global-set-key (kbd "<ESC> ptf") 'python-pytest-function)
(global-set-key (kbd "<ESC> ptp") 'python-pytest-popup)
(global-set-key (kbd "<ESC> ptr") 'python-pytest-repeat)

(defun pytest-mehmet ()
 (interactive)
 (python-pytest "--pdb"))


;; -------------------------------------  flycheck  ------------------------------------- ;;
;; Ref on configuring syntax checkers:
;; - https://www.flycheck.org/en/28/_downloads/flycheck.html#Configuring-checkers
;; - https://www.flycheck.org/en/27/_downloads/flycheck.html
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(add-to-list 'flycheck-disabled-checkers 'lsp t)
(add-to-list 'flycheck-disabled-checkers 'python-pylint t)
(add-to-list 'flycheck-disabled-checkers 'python-pycompile t)

;; --------------------------------  Python code formatting  -------------------------------- ;;
;; https://www.reddit.com/r/emacs/comments/supjnb/what_is_your_setup_for_python_coding_in_emacs/
;; (setenv "PYTHONIOENCODING" "utf8")

;; (setq python-shell-interpreter-args "-m asyncio")

;; (use-package python-black
;;     :ensure t
;;     :quelpa
;;     (python-black
;;         :fetcher git
;;         :url "https://github.com/wbolster/emacs-python-black")
;;     :after python)

;; (use-package python-black
;;   :demand t
;;   :after python
;;   :hook (python-mode . python-black-on-save-mode-enable-dwim))

;; (python-black-on-save-mode t)
;; (add-hook 'before-save-hook 'python-black-buffer)

;; (use-package py-isort
;;     :ensure t
;;     :quelpa
;;     (py-isort
;;          :fetcher git
;;          :url "https://github.com/paetzke/py-isort.el")
;;     )

;; (use-package lsp-pyright
;;     :ensure t
;;     :quelpa
;;     (lsp-pyright
;;         :fetcher git
;;         :url "https://github.com/emacs-lsp/lsp-pyright")
;;     :hook (python-mode . (lambda () (require 'lsp-pyright)))
;;     :init (when (executable-find "python3")
;;               (setq lsp-pyright-python-executable-cmd "python3")))

;; (defun my/python_pay_respects ()
;;     "Make string to fstring"
;;     (interactive)
;;     (when (nth 3 (syntax-ppss))
;;         (let ((p (point)))
;;              (goto-char (nth 8 (syntax-ppss)))
;;              (insert "f")
;;              (goto-char p)
;;              (forward-char))))

;; (defun my/python_format ()
;;     "Format python buffer with isort and black"
;;     (interactive)
;;     (py-isort-buffer)
;;     (python-black-buffer)
;;     (save-buffer))

;; (add-hook 'python-mode-hook
;;     (lambda () (local-set-key (kbd "C-c f") 'my/python_pay_respects)))

;; (add-hook 'python-mode-hook
;;     (lambda () (local-set-key (kbd "C-c r") 'my/python_format)))

(setq sh-basic-offset 2)

;; -------------------------  *** Rename variable across project ***  ------------------------- ;;
;; I now use helm-ag to find all instances of the function name (searches in all files, incl. subdirs,
;; not just in open buffers), and then I use C-c C-e to enter a buffer that lists all the matches
;; and there I change the function name. When I am done I press C-c C-c (helm-ag--edit-commit) to
;; store the changes to all the opened files. This might sound confusing but please see
;;
;; Ref:
;; - https://emacs.stackexchange.com/questions/7595/how-do-i-refactor-across-a-project-in-emacs-change-method-name-everywhere
;; - https://github.com/ShingoFukuyama/helm-swoop


;; ------------------------------------  Window splitting  ----------------------------------- ;;
;; Prefer to split window vertically:
;; Ref: https://stackoverflow.com/questions/23659909/reverse-evaluation-order-of-split-height-threshold-and-split-width-threshold-in
(defun my-split-window-sensibly (&optional window)
  (let ((window (or window (selected-window))))
    (or (and (window-splittable-p window t)
	     ;; Split window horizontally.
	     (with-selected-window window
	       (split-window-right)))
	(and (window-splittable-p window)
	     ;; Split window vertically.
	     (with-selected-window window
	       (split-window-below)))
	(and (eq window (frame-root-window (window-frame window)))
	     (not (window-minibuffer-p window))
	     ;; If WINDOW is the only window on its frame and is not the
	     ;; minibuffer window, try to split it horizontally disregarding
	     ;; the value of `split-width-threshold'.
	     (let ((split-width-threshold 0))
	       (when (window-splittable-p window t)
		 (with-selected-window window
		   (split-window-right))))))))

(setq split-window-preferred-function 'my-split-window-sensibly)

;; ------------------------------------  Keybindings  ----------------------------------- ;;
(straight-use-package 'find-file-in-project)
(straight-use-package 'helm)
(straight-use-package 'helm-ag)

;; https://github.com/redguardtoo/find-file-in-project
(require 'find-file-in-project)
(helm-mode 1)

;; Note: Follow these two guidelines while creating a key binding
;; 1. Run `describe-key` and press the key combination. This will show the
;; key combination that is read by Emacs. This combination can then be set
;; to a command like
;; (global-set-key (kbd "M-x") 'helm-M-x)
;;
;; 2. Run `Ctrl + q` then press the key combination. This will show the code
;; received by Emacs. The code can then be set to a command like
;; (global-set-key "[1;9A" 'helm-mini)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-s") 'save-buffer)
(global-set-key (kbd "M-q") 'quit-window)
(global-set-key (kbd "M-o") 'other-window)
;; (global-set-key (kbd "M-n") 'find-file-in-project)
(global-set-key (kbd "M-n") 'helm-projectile-find-file-dwim)
(global-set-key (kbd "M-f") 'find-file)
;; (global-set-key (kbd "M-g") 'helm-ag)
;; (global-set-key (kbd "M-g") 'helm-projectile-ag)
;; (global-set-key (kbd "M-g") 'helm-swoop)
(global-set-key (kbd "M-g") 'helm-ag-word-at-point)
(global-set-key (kbd "M-l") 'goto-line)
(global-set-key (kbd "M-h") 'highlight-symbol)
(global-set-key (kbd "<ESC> M-h") 'highlight-symbol-remove-all)
;; (global-set-key (kbd "M-b") 'helm-buffers-list)
(global-set-key (kbd "M-m") 'imenu)
(global-set-key (kbd "M-d") 'delete-window)
(global-set-key (kbd "M-j") 'jedi:goto-definition)
(global-set-key (kbd "M-=") 'move-text-line-up)
(global-set-key (kbd "M--") 'move-text-line-down)
(global-set-key (kbd "M-+") 'move-text-region-up)
(global-set-key (kbd "M-_") 'move-text-region-down)
(global-set-key (kbd "M-f") 'copy-line)
(global-set-key (kbd "M-c") 'copy-word)
(global-set-key (kbd "M-a") 'select-word)
;; (global-set-key (kbd "M-d") 'kill-line)
(global-set-key (kbd "M-b") 'back-to-indentation)
(global-set-key (kbd "M-e") 'yank)
(global-set-key (kbd "M-,") 'beginning-of-defun)
(global-set-key (kbd "M-.") 'end-of-defun)
;; (global-set-key (kbd "<prior>") 'beginning-of-defun)
;; (global-set-key (kbd "<next>") 'end-of-defun)
(global-set-key (kbd "<home>") 'beginning-of-defun)
(global-set-key (kbd "<end>") 'end-of-defun)
(global-set-key (kbd "ESC <up>") 'beginning-of-defun)
(global-set-key (kbd "ESC <down>") 'end-of-defun)

(global-set-key (kbd "\C-z") 'undo-fu-only-undo)
(global-set-key (kbd "\C-n") 'undo-fu-only-redo)
(global-set-key (kbd "\C-p") 'yank)
(global-set-key (kbd "\C-]") 'yank)
;; (global-set-key (kbd "\C-q") 'keyboard-quit)
;; (global-set-key (kbd "\C-h") 'keyboard-escape-quit)
;; (global-set-key (kbd "\C-o") 'isearch-yank-kill)
(global-set-key (kbd "\C-s") 'swiper)
(global-set-key (kbd "C-_") '
  comment-or-uncomment-region)
(global-set-key (kbd "C-j") 'jedi:complete)

(global-set-key (kbd "M-,") 'highlight-symbol-prev)
(global-set-key (kbd "M-.") 'highlight-symbol-next)

(global-set-key (kbd "M-;") 'iedit-mode)

;; (global-set-key (kbd "M-.") 'dumb-jump-go)
;; (global-set-key (kbd "M-,") 'dumb-jump-back)
(global-set-key "[1;9A" 'helm-mini) ;; (kbd "M-<up>")
(global-set-key "[1;9B" 'find-file) ;; (kbd "M-<down>")

;; (global-set-key "[1;10A" 'dumb-jump-go) ;; (kbd "M-S-<up>")
;; (global-set-key "[1;10B" 'dumb-jump-back) ;; (kbd "M-S-<down>")
(global-set-key "[1;10A" 'jedi:goto-definition) ;; (kbd "M-S-<up>")
(global-set-key "[1;10B" 'jedi:goto-definition-pop-marker) ;; (kbd "M-S-<down>")

(global-set-key "[1;10D" 'undo-fu-only-undo) ;; (kbd "M-S-<left>")
(global-set-key "[1;10C" 'undo-fu-only-redo) ;; (kbd "M-S-<right>")

;; (global-set-key (kbd "M-[") 'left-word)
;; (global-set-key (kbd "M-]") 'right-word)
;; (global-set-key (kbd "M-+") 'paragraph-forward)
;; (global-set-key (kbd "M-'") 'paragraph-backward)

;; (define-key key-translation-map (kbd "<left>") (kbd "["))
;; (define-key key-translation-map (kbd "[") (kbd "<left>"))
;; (define-key key-translation-map (kbd "<right>") (kbd "]"))
;; (define-key key-translation-map (kbd "]") (kbd "<right>"))
;; (define-key key-translation-map (kbd "<up>") (kbd "="))
;; (define-key key-translation-map (kbd "=") (kbd "<up>"))
;; (define-key key-translation-map (kbd "<down>") (kbd "'"))
;; (define-key key-translation-map (kbd "'") (kbd "<down>"))

;; (global-set-key (kbd "M-[") 'backward-paragraph)
;; (global-set-key (kbd "M-]") 'forward-paragraph)
;; (global-set-key (kbd "M-[") 'left-word)
;; (global-set-key (kbd "M-]") 'right-word)

;; (package-install 'flycheck)
;; (global-flycheck-mode)
;; (package-install 'exec-path-from-shell)
;; (exec-path-from-shell-initialize)

;; Move to beginning of line
;; Select a word
;; Prevent from copying while killing
;; Comment out a line without having to going to beginning of line

;; (setq x-select-enable-clipboard nil)
;; (setq x-select-enable-primary nil)
