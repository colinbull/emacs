;;-*-Emacs-Lisp-*-
;; .emacs

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
(setq package-enable-at-startup nil)

(require 'package)

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

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
    clojure-mode
    flycheck
    fsharp-mode
    gist
    haskell-mode
    idris-mode
    json
    markdown-mode
    rainbow-delimiters
    yasnippet)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;;------------------------------------------------------------------------------
;; General settings
;;------------------------------------------------------------------------------

(add-to-list 'load-path "~/.emacs.d/")
(setq debug-on-error t)

;; scroll from the keyboard (what is everyone else using!?)
(global-set-key "\M-N" 'up-semi-slow)
(global-set-key "\M-P" 'down-semi-slow)

;set theme
(load-theme 'zenburn t)

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


(setq auto-save-list-file-prefix "~/Appdev/.save/.saves-")


;;    Choose interactively from the kill ring.
(require 'browse-kill-ring)
(global-set-key (kbd "C-c C-k") 'browse-kill-ring)
;;(set-face-attribute 'default nil :height 140)

;;(set-cursor-color "#00ff00")

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

(require 'haskell-mode-autoloads)

;;------------------------------------------------------------------------------

(when window-system
;;  (speedbar 1)
;;  (eshell)
)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(highlight ((t nil))))
