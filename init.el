(require 'package)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/site-lisp/")

(setq inhibit-splash-screen t)

(setq split-height-threshold 999)
(setq split-width-threshhold 80)

(setq column-number-mode t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check if packages are installed and download any that are missing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar my-packages
  '(clang-format solarized-theme glsl-mode ghc color-theme haskell-mode)
  "A list of packages to ensure are installed at launch.")

(require 'cl-lib)

(defun my-packages-installed-p ()
  (cl-loop for p in my-packages
           when (not (package-installed-p p)) do (cl-return nil)
           finally (cl-return t)))

(unless (my-packages-installed-p)
  ;; check for new packages (package versions)
  (package-refresh-contents)
  ;; install the missing packages
  (dolist (p my-packages)
    (when (not (package-installed-p p))
      (package-install p))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fonts and colors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load-theme 'solarized-dark t)

(global-hl-line-mode 1)
(set-face-background 'hl-line "#ddd")

(if (string-match "darwin" system-configuration)
    (setq my-font "Inconsolata-11")
  (setq my-font "Inconsolata-9"))

(defun fontify-frame (frame)
  (set-frame-parameter frame 'font my-font))
(fontify-frame nil) ;;Set font for current frame
(push 'fontify-frame after-make-frame-functions) ;;Set font for future frames.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tramp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq tramp-default-method "ssh")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C/C++ and Qt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq c-default-style "python")
(setq c-basic-offset 4)
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

(require 'qt-pro)
(add-to-list 'auto-mode-alist '("\\.pr[io]$" . qt-pro-mode))

(add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))
(add-hook 'c-mode-common-hook '(lambda () (local-set-key (kbd "RET") 'newline-and-indent)))
(add-hook 'c-mode-common-hook (lambda () (linum-mode 1)))
(add-hook 'c-mode-common-hook (lambda () (setq indent-tabs-mode nil)))
(add-hook 'c-mode-common-hook 'flyspell-prog-mode)

(require 'clang-format)
(global-set-key (kbd "s-F") 'clang-format-buffer)

(setq clang-format-style-option "file")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OpenGL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'glsl-mode "glsl-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'org-mode-hook 'visual-line-mode)
(setq org-mobile-directory "~/Dropbox/MobileOrg")
(setq org-directory        "~/Dropbox/Org")
(setq org-mobile-inbox-for-pull "~/Dropbox/Org/from-mobile.org" )
(setq org-log-done 'time)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; .plist
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'auto-mode-alist '("\\.plist$" . xml-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Haskell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setenv "PATH" (concat "~/.cabal/bin:" (getenv "PATH")))
(add-to-list 'exec-path "~/.cabal/bin")
(custom-set-variables '(haskell-tags-on-save t))
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'haskell-mode-hook (lambda () (linum-mode 1)))
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(custom-set-variables
  '(haskell-process-type 'cabal-repl))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'python-mode-hook '(lambda () (local-set-key (kbd "RET") 'newline-and-indent)))
(add-hook 'python-mode-hook (lambda () (linum-mode 1)))
(add-hook 'python-mode-hook 'flyspell-prog-mode)


(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(tool-bar-mode nil))

(defun show-file-name ()
  "Show the full path of the file in the minibuffer."
  (interactive)
  (message (buffer-file-name)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Git
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'git-commit-mode-hook 'turn-on-flyspell)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'term-mode-hook 'compilation-shell-minor-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keyboard shortcuts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(setq mac-command-modifier 'super)
(setq mac-function-modifier 'hyper)

(global-set-key (kbd "C-s-|") (lambda () (interactive) (split-window-horizontally 100)))
(global-set-key (kbd "C-s-/") (lambda () (interactive) (split-window-horizontally 80)))

(global-set-key (kbd "s-B") 'compile)
(global-set-key (kbd "s-b") 'recompile)
(global-unset-key (kbd "s-o"))
(global-set-key (kbd "s-o") 'ff-find-other-file)
(global-set-key (kbd "s-r") 'revert-buffer)
(global-set-key (kbd "s-l") 'goto-line)

;;Unicode
(global-set-key (kbd "C-c u -") (lambda () (interactive) (insert "–"))) ;;en-dash
(global-set-key (kbd "C-c u _") (lambda () (interactive) (insert "—"))) ;;em-dash
(global-set-key (kbd "C-c u l") (lambda () (interactive) (insert "λ"))) ;;lambda
(global-set-key (kbd "C-c u c") (lambda () (interactive) (insert "⌘"))) ;;Mac command symbol

