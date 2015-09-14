;;-*-Emacs-Lisp-*-
;; .emacs

;;------------------------------------------------------------------------------
;; UI Disabling
;;------------------------------------------------------------------------------

(when window-system
  (scroll-bar-mode 0)
  (tool-bar-mode 0)
  (menu-bar-mode 0))

(setq inhibit-splash-screen t)
(setq-default truncate-lines 1)
(toggle-frame-fullscreen)
;;------------------------------------------------------------------------------
;; System type discriminators
;;------------------------------------------------------------------------------

(defvar system-type-as-string (prin1-to-string system-type))
(defvar on_windows_nt (string-match "windows-nt" system-type-as-string))
(defvar on_darwin     (string-match "darwin" system-type-as-string))
(defvar site-lisp-dir (expand-file-name "site-lisp" user-emacs-directory)
  "This directory is for your personal configuration.")

;;------------------------------------------------------------------------------
;; Load Path
;;------------------------------------------------------------------------------

(add-to-list 'load-path site-lisp-dir)

;;------------------------------------------------------------------------------
;; Package management
;;------------------------------------------------------------------------------

;; set the http_proxy environment variable if working behind a proxy

;; Initialize packages now so we can just require them
(require 'package)

(setq package-enable-at-startup nil)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ;("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
			 ("melpa-stable" . "http://stable.melpa.org/packages/")))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; Always install these packages

(defvar my-packages 
  '(
    zenburn-theme
    auto-compile
    auto-complete
    browse-kill-ring
    ido-at-point
    ido-ubiquitous
    ido-vertical-mode
    flycheck
    fsharp-mode
    omnisharp
    gist
    haskell-mode
    idris-mode
    json
    markdown-mode
    rainbow-delimiters
    yasnippet
 ;;   pretty-mode
    projectile
    flx-ido
    ace-jump-mode
    )
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(eval-after-load "sql"
  '(load-library "tsql-indent"))


;;------------------------------------------------------------------------------
;; General settings
;;------------------------------------------------------------------------------
(setq debug-on-error t)

;set theme
(load-theme 'zenburn t)

;enable line numbers
(global-linum-mode t)

;squash cd auto-save
(setq backup-directory-alist
          `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
          `((".*" ,temporary-file-directory t)))

;magic auto compile
(require 'auto-compile)
(auto-compile-on-load-mode 1)
(auto-compile-on-save-mode 1)

;;------------------------------------------------------------------------------
;; Keyboard
;;------------------------------------------------------------------------------

(global-set-key (kbd "M-3") '(lambda () (interactive) (insert "#")))
;; scroll from the keyboard (what is everyone else using!?)
(global-set-key "\M-N" 'up-semi-slow)
(global-set-key "\M-P" 'down-semi-slow)

;;set cycle buffers
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR.")
(global-set-key (kbd "M-z") 'zap-up-to-char)
;;-----------------------------------------------------------------------------
;; ACE Jump
;;-----------------------------------------------------------------------------

(require 'ace-jump-mode)
;; you can select the key you prefer to
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)


;;------------------------------------------------------------------------------
;; IDO
;;------------------------------------------------------------------------------

(require 'ido)
(ido-mode t)

(require 'ido-vertical-mode)
(ido-vertical-mode)

(global-set-key
     "\M-x"
     (lambda ()
       (interactive)
       (call-interactively
        (intern
         (ido-completing-read
          "M-x "
          (all-completions "" obarray 'commandp))))))

(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

;;------------------------------------------------------------------------------
;; Projectile
;;------------------------------------------------------------------------------

(require 'projectile)
(projectile-global-mode)

;;------------------------------------------------------------------------------
;; Auto Complete
;;------------------------------------------------------------------------------

(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/dict")
(setq-default ac-sources (add-to-list 'ac-sources 'ac-source-dictionary))
(global-auto-complete-mode t)
(setq ac-auto-start 2)
(setq ac-ignore-case nil)

(require 'yasnippet)
(yas-global-mode 1)
;; Add snippets to the auto-complete dropdown
(add-to-list 'ac-sources 'ac-source-yasnippet)

;;________________________________________________________________
;;    Insert hard newlines while typing in text mode
;;________________________________________________________________

(add-hook 'text-mode-hook '(lambda () (auto-fill-mode 1)))
(add-hook 'mail-mode-hook '(lambda () (auto-fill-mode 1)))
(add-hook 'latex-mode-hook '(lambda () (auto-fill-mode 1)))

;;________________________________________________________________
;;    Put all .save's in one place
;;________________________________________________________________

(setq auto-save-list-file-prefix "~/.save/.saves-")


;; Choose interactively from the kill ring.
(require 'browse-kill-ring)
(global-set-key (kbd "C-c C-k") 'browse-kill-ring)


;;------------------------------------------------------------------------------
;; Gist
;;------------------------------------------------------------------------------

;; Gist uses url.el which can be configured to use proxies

;;-----------------------------------------------------------------------------
;; Idris
;;----------------------------------------------------------------------------

(require 'idris-mode)

;;Point at idris dev sandbox
(add-to-list 'exec-path "~/.cabal/bin")


;;------------------------------------------------------------------------------
;;C#
;;-----------------------------------------------------------------------------

(require 'omnisharp)

(setq omnisharp--curl-executable-path "C://Users//colinbull//OneDrive//Tools//curl//winssl//curl.exe")
(setq omnisharp-server-executable-path "C://Appdev//OmniSharpServer//OmniSharp//bin//Debug//OmniSharp.exe")
(add-hook 'csharp-mode-hook 'omnisharp-mode)

;;------------------------------------------------------------------------------
;; F#
;;------------------------------------------------------------------------------

(require 'fsharp-mode)

; Compiler and REPL paths
(cond (on_darwin
       (setq inferior-fsharp-program "/usr/bin/fsharpi --readline-")
       (setq fsharp-compiler "/usr/bin/fsharpc")
       )
      (on_windows_nt
       (setq inferior-fsharp-program "\"C:\\Program Files (x86)\\Microsoft SDKs\\F#\\3.1\\Framework\\v4.0\\Fsi.exe\"")
       (setq fsharp-compiler "\"C:\\Program Files (x86)\\Microsoft SDKs\\F#\\3.1\\Framework\\v4.0\\Fsc.exe\"")
       ))

(add-hook 'fsharp-mode-hook
          (lambda ()
            (setq-default indent-tabs-mode nil)
            (setq-default tab-width 4)
            (setq indent-line-function 'insert-tab)
            (define-key fsharp-mode-map (kbd "M-RET") 'fsharp-eval-region)
            (define-key fsharp-mode-map (kbd "C-SPC") 'fsharp-ac/complete-at-point)))

(require 'pretty-mode)
(add-hook 'fsharp-mode-hook 'turn-on-pretty-mode)

;;------------------------------------------------------------------------------
;; XML
;;------------------------------------------------------------------------------

(setq auto-mode-alist (cons '("\\.ipe\\'" . xml-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.qrc\\'" . xml-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.svg\\'" . xml-mode) auto-mode-alist))

;;------------------------------------------------------------------------------
;; aspell
;;------------------------------------------------------------------------------

(add-to-list 'exec-path "/usr/local/bin/")
(setq ispell-program-name "aspell")
(require 'ispell)

;;------------------------------------------------------------------------------
;; markdown
;;------------------------------------------------------------------------------

(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

;;------------------------------------------------------------------------------
;; TeX
;;------------------------------------------------------------------------------

(setq TeX-auto-save t)
(setq TeX-parse-self t)

;;------------------------------------------------------------------------------
;; Haskell
;;------------------------------------------------------------------------------

(add-to-list 'load-path ".")
;; Always load via this. If you contribute you should run `make all`
;; to regenerate this.
(load "haskell-mode-autoloads")

;; Customization
(custom-set-variables
 ;; Use cabal-dev for the GHCi session. Ensures our dependencies are in scope.
 ;;'(haskell-process-type 'cabal-dev)

 ;; Use notify.el (if you have it installed) at the end of running
 ;; Cabal commands or generally things worth notifying.
 '(haskell-notify-p t)

 ;; To enable tags generation on save.
 '(haskell-tags-on-save t)

 ;; To enable stylish on save.
 '(haskell-stylish-on-save t))

(add-hook 'haskell-mode-hook 'haskell-hook)
(add-hook 'haskell-cabal-mode-hook 'haskell-cabal-hook)

;; Haskell main editing mode key bindings.
(defun haskell-hook ()
  ;; Use simple indentation.
  (turn-on-haskell-simple-indent)
  (define-key haskell-mode-map (kbd "<return>") 'haskell-simple-indent-newline-same-col)
  (define-key haskell-mode-map (kbd "C-<return>") 'haskell-simple-indent-newline-indent)

  ;; Load the current file (and make a session if not already made).
  (define-key haskell-mode-map [?\C-c ?\C-l] 'haskell-process-load-file)
  (define-key haskell-mode-map [f5] 'haskell-process-load-file)

  ;; Switch to the REPL.
  (define-key haskell-mode-map [?\C-c ?\C-z] 'haskell-interactive-switch)
  ;; “Bring” the REPL, hiding all other windows apart from the source
  ;; and the REPL.
  (define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)

  ;; Build the Cabal project.
  (define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
  ;; Interactively choose the Cabal command to run.
  (define-key haskell-mode-map (kbd "C-c c") 'haskell-process-cabal)

  ;; Get the type and info of the symbol at point, print it in the
  ;; message buffer.
  (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
  (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)

  ;; Contextually do clever things on the space key, in particular:
  ;;   1. Complete imports, letting you choose the module name.
  ;;   2. Show the type of the symbol after the space.
  (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)

  ;; Jump to the imports. Keep tapping to jump between import
  ;; groups. C-u f8 to jump back again.
  (define-key haskell-mode-map [f8] 'haskell-navigate-imports)

  ;; Jump to the definition of the current symbol.
  (define-key haskell-mode-map (kbd "M-.") 'haskell-mode-tag-find)

  ;; Indent the below lines on columns after the current column.
  (define-key haskell-mode-map (kbd "C-<right>")
    (lambda ()
      (interactive)
      (haskell-move-nested 1)))
  ;; Same as above but backwards.
  (define-key haskell-mode-map (kbd "C-<left>")
    (lambda ()
      (interactive)
      (haskell-move-nested -1))))

;; Useful to have these keybindings for .cabal files, too.
(defun haskell-cabal-hook ()
  (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
  (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)
  (define-key haskell-cabal-mode-map (kbd "C-`") 'haskell-interactive-bring)
  (define-key haskell-cabal-mode-map [?\C-c ?\C-z] 'haskell-interactive-switch))

;;------------------------------------------------------------------------------

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(highlight ((t nil))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#3F3F3F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCCC"])
 '(custom-safe-themes (quote ("9dae95cdbed1505d45322ef8b5aa90ccb6cb59e0ff26fef0b8f411dfc416c552" default)))
 '(fci-rule-color "#383838")
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map (quote ((20 . "#BC8383") (40 . "#CC9393") (60 . "#DFAF8F") (80 . "#D0BF8F") (100 . "#E0CF9F") (120 . "#F0DFAF") (140 . "#5F7F5F") (160 . "#7F9F7F") (180 . "#8FB28F") (200 . "#9FC59F") (220 . "#AFD8AF") (240 . "#BFEBBF") (260 . "#93E0E3") (280 . "#6CA0A3") (300 . "#7CB8BB") (320 . "#8CD0D3") (340 . "#94BFF3") (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3"))
