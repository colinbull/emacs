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
    auto-compile
    auto-complete
    browse-kill-ring
    doremi-cmd
    flycheck
    fsharp-mode
    haskell-mode
    markdown-mode
    starter-kit
    starter-kit-lisp
    starter-kit-bindings
    starter-kit-js
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

(setq require-final-newline 'query)

(setq special-display-buffer-names
        (nconc '("*Backtrace*" "*VC-log*" "*compilation*" "*grep*")
               special-display-buffer-names))

;; If we read a compressed file, uncompress it on the fly:
;; (this works with .tar.gz and .tgz file as well)
(auto-compression-mode 1)

;; Disable iconify window
(global-unset-key "\C-x\C-z")
(global-unset-key "\C-z")

;; Disable kanji
(global-unset-key "\C-xj")

;; Disable tmm-menubar
(global-unset-key "\M-`")

;; Disable set-goal-column
(global-unset-key "\C-x\C-n")

;; Emacs Shell
(global-set-key "\C-c\C-z" 'eshell)

;; Prevent down-arrow from adding empty lines to the bottom of the buffer
(setq next-line-add-newlines nil)

;; Flash the screen on error; don't beep.
(setq-default visible-bell t)

;; Highlight the marked region.
(setq-default transient-mark-mode t)

;; 'woman' mode is an improvement on 'man' mode for manual pages
(setq-default woman-use-own-frame nil)
(setq-default Man-notify-method 'pushy)

;; Permanent display of line and column numbers is handy.
(setq-default line-number-mode 't)
(setq-default column-number-mode 't)

;; Don't display initial logo
(setq inhibit-startup-message t)

;; Display long lines by truncating them
(set-default 'truncate-lines   t)

;; Show matching parenthesis
(show-paren-mode t)

;; hi lock is only available since Emacs 22.
(when (>= emacs-major-version 22)
  (global-hi-lock-mode 1)
  (setq hi-lock-file-patterns-policy t)
  )

(local-set-key "\M-\C-g" 'org-plot/gnuplot)

;; Disable menu bar
(cond (on_windows_nt
       (menu-bar-mode -1)
       ))

;; Disable tool bar
(when window-system
  (tool-bar-mode -1)
  (server-start)
  )

(add-hook 'server-switch-hook
            (lambda ()
              (when (current-local-map)
                (use-local-map (copy-keymap (current-local-map))))
	      (when server-buffer-clients
		(local-set-key (kbd "C-x k") 'server-edit))))

(defun swap-windows ()
  "If you have 2 windows, it swaps them."
  (interactive)
  (cond
   ((not (= (count-windows) 2)) (message "You need exactly 2 windows to do this."))
   (t
    (let* ((w1 (first (window-list)))
           (w2 (second (window-list)))
           (b1 (window-buffer w1))
           (b2 (window-buffer w2))
           (s1 (window-start w1))
           (s2 (window-start w2)))
      (set-window-buffer w1 b2)
      (set-window-buffer w2 b1)
      (set-window-start w1 s2)
      (set-window-start w2 s1)))))

(require 'auto-compile)
(auto-compile-on-load-mode 1)
(auto-compile-on-save-mode 1)


;;------------------------------------------------------------------------------
;; Alternative visual bell
;;------------------------------------------------------------------------------

(defcustom echo-area-bell-string "*DING* " ;"♪"
 "Message displayed in mode-line by `echo-area-bell' function."
 :group 'user)
(defcustom echo-area-bell-delay 0.1
 "Number of seconds `echo-area-bell' displays its message."
 :group 'user)
;; internal variables
(defvar echo-area-bell-cached-string nil)
(defvar echo-area-bell-propertized-string nil)
(defun echo-area-bell ()
 "Briefly display a highlighted message in the echo-area.
The string displayed is the value of `echo-area-bell-string',
with a red background; the background highlighting extends to the
right margin.  The string is displayed for `echo-area-bell-delay'
seconds.
This function is intended to be used as a value of `ring-bell-function'."
 (unless (equal echo-area-bell-string echo-area-bell-cached-string)
   (setq echo-area-bell-propertized-string
         (propertize
          (concat
           (propertize
            "x"
            'display
            `(space :align-to (- right ,(+ 2 (length echo-area-bell-string)))))
           echo-area-bell-string)
          'face '(:background "red")))
   (setq echo-area-bell-cached-string echo-area-bell-string))
 (message echo-area-bell-propertized-string)
 (sit-for echo-area-bell-delay)
 (message ""))
(setq ring-bell-function 'echo-area-bell)

;;------------------------------------------------------------------------------
;;    Fonts and Colours
;;------------------------------------------------------------------------------

(require 'doremi-cmd)

;; Use font-lock everywhere.
(global-font-lock-mode t)

;; To the max!
(setq font-lock-maximum-decoration t)

;; Dark background
(set-foreground-color "white")
(set-background-color "black")

;; Set cursor and mouse colours:
;;(set-cursor-color "yellow")
(set-mouse-color "white")

(cond (on_darwin
       (set-default-font "-apple-monaco-medium-r-normal--14-*-*-*-*-*-*-*")
       ;; (set-default-font "-apple-monaco-medium-r-normal--12-120-72-72-m-120-iso10646-1")
       ;; (set-default-font "-apple-monaco-medium-r-normal--13-130-72-72-m-130-iso10646-1")
       ;; (set-default-font "-apple-monaco-medium-r-normal--14-140-72-72-m-140-iso10646-1")
       ;;   (set-default-font "-apple-monaco-medium-r-normal--18-*-*-*-*-*-*-*")
       ;;   (set-default-font "-apple-monaco-medium-r-normal--24-*-*-*-*-*-*-*")
       ;;   (set-default-font "-apple-monaco-medium-r-normal--36-*-*-*-*-*-*-*")
))

(cond (on_windows_nt
       ;; (set-default-font "Lucida Console-9:bold")
       ;;(set-default-font "Lucida Console-11:bold")
       ;; (set-default-font "Lucida Console-13:bold")
       ;; (set-default-font "Lucida Console-15:bold")
       (set-default-font "Consolas-11")
       ;; (set-default-font "Courier New-12:bold")
))

(load-theme 'deeper-blue)

;;------------------------------------------------------------------------------
;; Scrolling
;;------------------------------------------------------------------------------

;; We also map scroll wheel and trackpad events to scrolling.
;; The mouse wheel on windows generates few events.
;; Scroll by 3 unless shifted.

(defun up-slow () (interactive) (scroll-up 1))
(defun down-slow () (interactive) (scroll-down 1))

(defun up-semi-slow () (interactive) (scroll-up 2))
(defun down-semi-slow () (interactive) (scroll-down 2))

(defun up-medium () (interactive) (scroll-up 3))
(defun down-medium () (interactive) (scroll-down 3))

(cond (on_windows_nt
       ;; xemacs won't like the following:
       (global-set-key [mouse-4] 'down-medium)
       (global-set-key [mouse-5] 'up-medium)

       (global-set-key [S-mouse-4] 'down-slow)
       (global-set-key [S-mouse-5] 'up-slow)
))

;; The trackpad on Mac OSX generates too many events.
;; Scroll by 1 unless shifted.
(cond (on_darwin
       (global-set-key [mouse-4] 'down-slow)
       (global-set-key [mouse-5] 'up-slow)

       (global-set-key [S-mouse-4] 'down-medium)
       (global-set-key [S-mouse-5] 'up-medium)
))

(defun up-fast () (interactive) (scroll-up 8))
(defun down-fast () (interactive) (scroll-down 8))
(global-set-key [C-mouse-4] 'down-fast)
(global-set-key [C-mouse-5] 'up-fast)

;; Ordinarily emacs jumps by half a page when scrolling -- reduce:
(setq scroll-step 1)

;; The default value is 5, which is too fast on a MacBook or a trackpad; reduce:
(cond (on_darwin
       (mouse-wheel-mode 1)
       (setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
       (setq mouse-wheel-progressive-speed 'f)
))

;; And finally, the most useful addition to .emacs: the ability to
;; scroll from the keyboard (what is everyone else using!?)
(global-set-key "\M-N" 'up-semi-slow)
(global-set-key "\M-P" 'down-semi-slow)

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
(yas--initialize)
;; Add snippets to the auto-complete dropdown
(add-to-list 'ac-sources 'ac-source-yasnippet)

;;------------------------------------------------------------------------------
;; Dired
;;------------------------------------------------------------------------------

;; A few customizations:
;; Among them: make copy and delete in dired recursive.

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(dired-recursive-copies (quote top))
 '(dired-recursive-deletes (quote top))
 '(large-file-warning-threshold 100000000)
 '(mumamo-submode-indent-offset 4)
 '(same-window-buffer-names (quote ("*eshell*" "*Python*" "*shell*"
                                    "*mail*" "*inferior-lisp*"
                                    "*ielm*" "*scheme*" "*"))))

;;________________________________________________________________
;;    Insert hard newlines while typing in text mode
;;________________________________________________________________

(add-hook 'text-mode-hook '(lambda () (auto-fill-mode 1)))
(add-hook 'mail-mode-hook '(lambda () (auto-fill-mode 1)))
(add-hook 'latex-mode-hook '(lambda () (auto-fill-mode 1)))

;;________________________________________________________________
;;    Put all .save's in one place
;;________________________________________________________________

(cond (on_darwin
       (setq auto-save-list-file-prefix "~/temp/misc/.save/.saves-" )
))

;;    Choose interactively from the kill ring.
(require 'browse-kill-ring)
(global-set-key (kbd "C-c C-k") 'browse-kill-ring)
;;(set-face-attribute 'default nil :height 140)

(set-cursor-color "#00ff00")

;;------------------------------------------------------------------------------
;; F#
;;------------------------------------------------------------------------------

(unless (package-installed-p 'fsharp-mode)
  (package-install 'fsharp-mode))

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
   (define-key fsharp-mode-map (kbd "M-RET") 'fsharp-eval-region)
   (define-key fsharp-mode-map (kbd "C-SPC") 'fsharp-ac/complete-at-point)))

;;------------------------------------------------------------------------------
;; Javascript
;;------------------------------------------------------------------------------

(require 'flycheck)

(cond (on_windows_nt       
       (flycheck-define-checker javascript-jslint-reporter
         "A JavaScript syntax and style checker based on JSLint Reporter. See URL `https://github.com/FND/jslint-reporter'."
         :command ("~/.emacs.d/jslint-reporter/jslint-reporter.bat" source)
         :error-patterns ((error line-start (1+ nonl) ":" line ":" column ":" (message) line-end))
         :modes (js-mode js2-mode js3-mode)))
      (on_darwin
       (flycheck-define-checker javascript-jslint-reporter
         "A JavaScript syntax and style checker based on JSLint Reporter. See URL `https://github.com/FND/jslint-reporter'."
         :command ("~/.emacs.d/jslint-reporter/jslint-reporter" source)
         :error-patterns ((error line-start (1+ nonl) ":" line ":" column ":" (message) line-end))
         :modes (js-mode js2-mode js3-mode))))

(add-hook 'js-mode-hook (lambda ()
                          (flycheck-select-checker 'javascript-jslint-reporter)
                          (flycheck-mode)))

(require 'js-comint) 
(setq inferior-js-program-command "node") ;; not "node-repl"
;;Use your favorited js mode here:
(setq inferior-js-mode-hook
      (lambda ()
        ;; We like nice colors
        (ansi-color-for-comint-mode-on)
        ;; Deal with some prompt nonsense
        (add-to-list 'comint-preoutput-filter-functions
                     (lambda (output)
                       (replace-regexp-in-string ".*1G\.\.\..*5G" "..."
                       (replace-regexp-in-string ".*1G.*3G" "> " output))))))

(add-hook 'js-mode-hook '(lambda ()
                           (setq indents-tab-mode t tab-width 4 js-indent-level 4)
                           (local-set-key "\C-x\C-e" 
                                          'js-send-last-sexp)
                           (local-set-key "\C-\M-x" 
                                          'js-send-last-sexp-and-go)
                           (local-set-key "\C-cb" 
                                          'js-send-buffer)
                           (local-set-key "\C-c\C-b" 
                                          'js-send-buffer-and-go)
                           (local-set-key "\C-cl" 
                                          'js-load-file-and-go)
                           ))

(add-hook 'js-mode-hook
          (lambda ()
            ;; Scan the file for nested code blocks
            (imenu-add-menubar-index)
            ;; Activate the folding mode
            (hs-minor-mode t)))

;;------------------------------------------------------------------------------
;; XML
;;------------------------------------------------------------------------------

(setq auto-mode-alist (cons '("\\.ipe\\'" . xml-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.qrc\\'" . xml-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.svg\\'" . xml-mode) auto-mode-alist))

;;------------------------------------------------------------------------------


;; aspell
(add-to-list 'exec-path "/usr/local/bin/")
(setq ispell-program-name "aspell")
(require 'ispell)

;; markdown
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

;; TeX
(setq TeX-auto-save t)
(setq TeX-parse-self t)

;; Octave
(autoload 'octave-mode "octave-mod" nil t)
(setq auto-mode-alist
      (cons '("\\.m$" . octave-mode) auto-mode-alist))

(add-hook 'octave-mode-hook
          (lambda ()
            (abbrev-mode 1)
            (auto-fill-mode 1)
            (if (eq window-system 'x)
                (font-lock-mode 1))))

;; Allow hash to be entered  
(global-set-key (kbd "M-3") '(lambda () (interactive) (insert "#")))

;;------------------------------------------------------------------------------
;; Haskell
;;------------------------------------------------------------------------------

(require 'haskell-mode-autoloads)



;;------------------------------------------------------------------------------


(when window-system
;;  (speedbar 1)
;;  (eshell)
  )

