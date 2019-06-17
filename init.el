(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(add-to-list 'load-path "~/.emacs.d/themes/")
(add-to-list 'load-path "~/.emacs.d/lisp/")

(require 're-builder)
(setq reb-re-syntax 'string)

; Keybinds
(global-set-key (kbd "C-;") 'other-window)
(global-set-key (kbd "M-;") (lambda() (interactive) (other-window -1)))
(global-set-key (kbd "M-[") 'previous-buffer)
(global-set-key (kbd "M-]") 'next-buffer)

;(define-key flyspell-mode-map (kbd "C-;") nil) ; unbind in flyspell

(global-set-key (kbd "M-\"") 'insert-pair) ;Wrap quotes
(global-set-key (kbd "C-x g") 'magit-status)
;(define-key org-mode-map "M-q" 'toggle-truncate-lines)
(global-set-key [remap dabbrev-expand] 'hippie-expand)

; Tabs. TODO: which of these does stuff?!
(setq-default indent-tabs-mode nil)
(setq-default js2-tab-width 2)
(setq-default tab-width 2)
(setq-default js2-basic-offset 2)
(setq js-indent-level 2)

; Font
(setq-default line-spacing 0.12)

; Dired
(setq dired-dwim-target t)
(add-hook 'dired-mode-hook
          (lambda()
            (dired-hide-details-mode))) ; Hide dired detailsn
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

; neotree
(add-to-list 'load-path "~/.emacs.d/lisp/emacs-neotree")
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))
(setq neo-smart-open t)

; y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;;; backup/autosave
(defvar backup-dir (expand-file-name "~/.emacs.d/backups/"))
(defvar autosave-dir (expand-file-name "~/.emacs.d/autosave/"))
(setq backup-directory-alist (list (cons ".*" backup-dir)))
(setq auto-save-list-file-prefix autosave-dir)
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))

;;  Modes

;; Ido
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
(setq ido-create-new-buffer 'always)
(setq ido-use-filename-at-point 'guess)

;; Hooks
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

;; (add-hook 'prog-mode-hook 'linum-mode)
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))

;(eval-after-load 'flycheck
;  '(add-hook 'flycheck-mode-hook #'flycheck-typescript-tslint-setup))

(setq flycheck-javascript-eslint-executable "/home/mrspeaker/.nvm/versions/node/v11.1.0/bin/eslint")
;(setq flycheck-javascript-tslint-executable "/home/mrspeaker/.nvm/versions/node/v11.1.0/bin/tslint")

; TIDE
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; (add-hook 'before-save-hook 'tide-format-before-save) - oh nope - bad formating.
(add-hook 'typescript-mode-hook #'setup-tide-mode)

(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "jsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))

;; (flycheck-add-mode 'javascript-eslint 'web-mode) - check this - no such thing?
; (flycheck-add-next-checker 'javascript-eslint 'jsx-tide 'append)
; (flycheck-add-mode 'typescript-tslint 'web-mode)
(add-hook 'js2-mode-hook (lambda ()
                           (tern-mode)
                           (company-mode)))

(require 'prettier-js)
(setq prettier-js-command "/home/mrspeaker/.nvm/versions/node/v11.1.0/bin/prettier")
(add-hook 'js2-mode-hook (lambda()
                           (prettier-js-mode)
                           ))
(add-hook 'typescript-mode-hook (lambda()
                           (prettier-js-mode)
                           ))
;(add-hook 'web-mode-hook 'prettier-js-mode)
;(add-hook 'js2-mode-hook 'flow-minor-enable-automatically)

(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'after-init-hook
          (lambda ()
            (progn
              (whole-line-or-region-global-mode)
              (when (fboundp 'auto-dim-other-buffers-mode)
                (auto-dim-other-buffers-mode t)))))

(require 'sunshine) ; Weather: sunshine-forecast

(require 'yasnippet)
(yas-global-mode 1)

; erc
(setq erc-lurker-hide-list '("JOIN" "PART" "QUIT"))
(setq erc-lurker-threshold-time 3600)

; TODO: In the process of moving init.el to an org file
(when (file-readable-p "~/.emacs.d/config.org")
  (org-babel-load-file (expand-file-name "~/.emacs.d/config.org")))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ange-ftp-passive-host-alist (quote (("ftp\\.mrspeaker\\.net" . "on"))))
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(auto-dim-other-buffers-mode t)
 '(beacon-color "#cc6666")
 '(company-backends
   (quote
    (company-tern company-tide company-bbdb company-eclim company-semantic company-clang company-xcode company-cmake company-capf company-files
                  (company-dabbrev-code company-gtags company-etags company-keywords)
                  company-oddmuse company-dabbrev)))
 '(css-indent-offset 2)
 '(custom-enabled-themes (quote (gruvbox-light-hard)))
 '(custom-safe-themes
   (quote
    ("a22f40b63f9bc0a69ebc8ba4fbc6b452a4e3f84b80590ba0a92b4ff599e53ad0" "585942bb24cab2d4b2f74977ac3ba6ddbd888e3776b9d2f993c5704aa8bb4739" "2b9dc43b786e36f68a9fd4b36dd050509a0e32fe3b0a803310661edb7402b8b6" "b583823b9ee1573074e7cbfd63623fe844030d911e9279a7c8a5d16de7df0ed0" "8e797edd9fa9afec181efbfeeebf96aeafbd11b69c4c85fa229bb5b9f7f7e66c" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "e68cf26c0e2957f19cd8e857d9546b17a32ded41caa00a4f94c8276b2c19cb2b" "217ad4914762a1ff7dfb7dc55df85e37c11755fb2bb0c8a5df272e425bf669d9" "f87f74ecd2ff6dc433fb4af4e76d19342ea4c50e4cd6c265b712083609c9b567" default)))
 '(desktop-save (quote ask-if-new))
 '(fci-rule-color "#373b41")
 '(flycheck-color-mode-line-face-to-color (quote mode-line-buffer-id))
 '(flycheck-typescript-tslint-config "tsconfig.json")
 '(flycheck-typescript-tslint-executable "/home/mrspeaker/.nvm/versions/node/v11.1.0/bin/tslint")
 '(frame-background-mode (quote dark))
 '(hl-paren-colors
   (quote
    ("#B9F" "#B8D" "#B7B" "#B69" "#B57" "#B45" "#B33" "#B11")))
 '(md4rd-subs-active (quote (emacs gamedev spacex orgmode fortnitebr)))
 '(neo-show-slash-for-folder nil)
 '(org-agenda-files (quote ("~/work.org")))
 '(org-startup-indented t)
 '(org-startup-with-inline-images t)
 '(package-selected-packages
   (quote
    (php-mode gruvbox-theme company-tern tern company tide md4rd auto-dim-other-buffers all-the-icons-dired all-the-icons markdown-mode flycheck slack typescript-mode magit yasnippet-snippets yasnippet expand-region hackernews org-download url-http-ntlm js2-mode)))
 '(sunshine-appid "4a4924c8eb9b826d500afed6ea276dce")
 '(sunshine-location "11217,US")
 '(sunshine-show-icons t)
 '(sunshine-units (quote metric))
 '(tool-bar-mode nil)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#cc6666")
     (40 . "#de935f")
     (60 . "#f0c674")
     (80 . "#b5bd68")
     (100 . "#8abeb7")
     (120 . "#81a2be")
     (140 . "#b294bb")
     (160 . "#cc6666")
     (180 . "#de935f")
     (200 . "#f0c674")
     (220 . "#b5bd68")
     (240 . "#8abeb7")
     (260 . "#81a2be")
     (280 . "#b294bb")
     (300 . "#cc6666")
     (320 . "#de935f")
     (340 . "#f0c674")
     (360 . "#b5bd68"))))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Liberation Mono" :foundry "1ASC" :slant normal :weight normal :height 120 :width normal))))
 '(auto-dim-other-buffers-face ((t (:background "#111"))))
 '(sunshine-forecast-headline-face ((t (:foreground "navajo white" :height 1.1)))))
