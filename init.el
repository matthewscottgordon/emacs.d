(setq frame-inhibit-implied-resize t) ;; prevent resize window on startup
(setq default-frame-alist '((width . 80) (height . 30)))
(tool-bar-mode -1)

(require 'package)

(setq package-archives
      '(("melpa"   .       "https://melpa.org/packages/")))

(package-initialize)
;;(package-refresh-contents)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

(setq use-package-always-ensure t)

(add-to-list 'load-path "~/.emacs.d/site-lisp/")

(use-package which-key
  :diminish
  :config
  (which-key-mode))

(require 'uniquify)
(setq uniquify-separator "/"               ;; The separator in buffer names.
      uniquify-buffer-name-style 'forward) ;; names/in/this/style

(setq inhibit-splash-screen t)

(setq split-height-threshold 999)
(setq split-width-threshhold 80)

(setq column-number-mode t)

(defun show-file-name ()
  "Show the full path of the file in the minibuffer."
  (interactive)
  (message (buffer-file-name)))

(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

(use-package ace-window)
(global-set-key (kbd "C-x o") 'ace-window)
(global-set-key (kbd "C-c o") 'ace-window)

(setq-default tab-width 4)
(setq-default indent-tab-mode nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(setq org-agenda-files (list "~/org/journal.org"
							 "~/org/refile.org"))
(setq org-todo-keywords (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)"))))
(global-set-key (kbd "C-c c") 'org-capture)
(setq org-capture-templates
	  (quote (("t" "To-do" (file "~/org/refile.org")
			   "* TODO %?\n%U\n" :clock-in t :clock-resume t)
			  ("c" "Call" (file "~/org/refile.org")
			   "* Call: %U %?" :clock-in t :clock-resume t)
			  ("j" "Journal" (file "~/org/refile.org")
			   "* %? %U" :clock-in t :clock-resume t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fonts and colors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package dakrone-light-theme)
(load-theme 'dakrone-light t)

(global-hl-line-mode 1)
(set-face-background 'hl-line "#ddd")

(setq my-font "Inconsolata-14")

(defun fontify-frame (frame)
  (set-frame-parameter frame 'font my-font))
(fontify-frame nil) ;;Set font for current frame
(push 'fontify-frame after-make-frame-functions) ;;Set font for future frames.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tramp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq tramp-default-method "ssh")




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rust
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package rust-mode)
(add-hook 'rust-mode-hook
          (lambda () (setq indent-tabs-mode nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Typescript and React
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Define typescript-tsx-mode
;; This was taken from a github issue form emacs-typescript; there's a PR to
;; merge a version of this code into typescript-mode itself so I should be able
;; to remove it from here in the future.
;; https://github.com/emacs-typescript/typescript.el/issues/4#issuecomment-873485004
;; https://github.com/emacs-typescript/typescript.el/pull/155

(use-package typescript-mode
  :ensure t
  :init
  (define-derived-mode typescript-tsx-mode typescript-mode "tsx")
  :config
  (setq typescript-indent-level 2)
  (add-hook 'typescript-mode #'subword-mode)
  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescript-tsx-mode)))

(use-package tree-sitter
  :ensure t
  :hook ((typescript-mode . tree-sitter-hl-mode)
	 (typescript-tsx-mode . tree-sitter-hl-mode)))

(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter
  :config
  (tree-sitter-require 'tsx)
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescript-tsx-mode . tsx)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Svelte
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package svelte-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C, C++, CMake and Qt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; (use-package clang-format)
;; (use-package irony)

;; (setq c-default-style "python")
;; (setq c-basic-offset 4)
;; (setq-default tab-width 4)
;; (setq-default indent-tabs-mode nil)

;; (require 'qt-pro)
;; (add-to-list 'auto-mode-alist '("\\.pr[io]$" . qt-pro-mode))

;; (add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))
;; (add-hook 'c-mode-common-hook
;;           '(lambda () (local-set-key (kbd "RET") 'newline-and-indent)))
;; (add-hook 'c-mode-common-hook (lambda () (linum-mode 1)))
;; (add-hook 'c-mode-common-hook (lambda () (setq indent-tabs-mode nil)))
;; (add-hook 'c-mode-common-hook 'flyspell-prog-mode)
;; (add-hook 'c++-mode-hook 'irony-mode)
;; (add-hook 'c-mode-hook 'irony-mode)

;; (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

(add-hook 'c-mode-common-hook (lambda () (linum-mode 1)))

(require 'clang-format)
(add-hook 'c-mode-common-hook
          (lambda () (local-set-key (kbd "C-c C-f") 'clang-format-buffer)))

(setq clang-format-executable "/usr/bin/clang-format-11")
(setq clang-format-style-option "file")

(setq cmake-tab-width 4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OpenGL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package glsl-mode)
(autoload 'glsl-mode "glsl-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Markdown
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package markdown-mode)
(setq markdown-command
      (concat
       "pandoc"
       " --from=gfm --to=html"
       " --quiet"
       ))
(setq browse-url-browser-function 'eww-browse-url)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; .plist
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'auto-mode-alist '("\\.plist$" . xml-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'python-mode-hook '(lambda () (local-set-key (kbd "RET") 'newline-and-indent)))
(add-hook 'python-mode-hook (lambda () (linum-mode 1)))
(add-hook 'python-mode-hook 'flyspell-prog-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Git
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package git-commit)
(add-hook 'git-commit-mode-hook 'turn-on-flyspell)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'term-mode-hook 'compilation-shell-minor-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keyboard shortcuts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(setq mac-command-modifier 'super)
(setq mac-function-modifier 'hyper)

(global-set-key (kbd "C-s-|")
                (lambda () (interactive) (split-window-horizontally 90)))
(global-set-key (kbd "C-s-#")
                (lambda () (interactive) (set-frame-width (selected-frame) 273)
                  (select-window (split-window-horizontally 90))
                  (split-window-horizontally 90)))
(global-set-key (kbd "C-s-$")
                (lambda () (interactive) (set-frame-width (selected-frame) 365)
                  (select-window (split-window-horizontally 92))
                  (select-window (split-window-horizontally 92))
                  (split-window-horizontally 92)))

(global-set-key (kbd "s-B") 'compile)
(global-set-key (kbd "s-b") 'recompile)
(global-unset-key (kbd "s-o"))
(global-set-key (kbd "s-o") 'ff-find-other-file)
(global-set-key (kbd "s-r") 'revert-buffer)
(global-set-key (kbd "s-l") 'goto-line)

;;Unicode
(global-set-key (kbd "C-c u -")
                (lambda () (interactive) (insert "–"))) ;;en-dash
(global-set-key (kbd "C-c u _")
                (lambda () (interactive) (insert "—"))) ;;em-dash
(global-set-key (kbd "C-c u l")
                (lambda () (interactive) (insert "λ"))) ;;lambda
(global-set-key (kbd "C-c u c")
                (lambda () (interactive) (insert "⌘"))) ;;Mac command symbol


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Local settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((local-settings "~/.emacs.d/local.el"))
  (when (file-exists-p local-settings)
	(load local-settings)))

(setq custom-file "~/.emacs.d/customize.el")
(load custom-file :noerror)

