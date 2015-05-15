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
;;------------------------------------------------------------------------------
;; System type discriminators
;;------------------------------------------------------------------------------

(defvar system-type-as-string (prin1-to-string system-type))
(defvar on_windows_nt (string-match "windows-nt" system-type-as-string))
(defvar on_darwin     (string-match "darwin" system-type-as-string))

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
    flycheck
    fsharp-mode
    gist
   ;; haskell-mode
    idris-mode
    json
    markdown-mode
    rainbow-delimiters
    yasnippet
    pretty-mode
    projectile
    flx-ido
    )
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

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

;; scroll from the keyboard (what is everyone else using!?)
(global-set-key "\M-N" 'up-semi-slow)
(global-set-key "\M-P" 'down-semi-slow)

;;set cycle buffers
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)

;;------------------------------------------------------------------------------
;; IDO
;;------------------------------------------------------------------------------

(require 'ido)
(ido-mode t)

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
(add-to-list 'exec-path "~/Appdev/idris/.cabal-sandbox/bin")



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

;;(require 'haskell-mode-autoloads)

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
