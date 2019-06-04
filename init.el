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
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))

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

;(eval-after-load 'flycheck
;  '(add-hook 'flycheck-mode-hook #'flycheck-typescript-tslint-setup))

(setq flycheck-javascript-eslint-executable "/home/mrspeaker/.nvm/versions/node/v11.1.0/bin/eslint")
;(setq flycheck-javascript-tslint-executable "/home/mrspeaker/.nvm/versions/node/v11.1.0/bin/tslint")


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

; Org mode
(setq org-todo-keywords
      '((sequence "TODO" "WIP" "DONE")))

;; Don't smash frames
(setq org-agenda-window-setup 'current-window)

;; Babel exec code
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (js . t)
   ))
(setq org-src-fontify-natively t) ; syntax highlight
(setq org-confirm-babel-evaluate nil) ; don't ask to exec
(setq org-src-tab-acts-natively t) ; tab in code blocks

; Org-capture
(setq org-capture-templates
     '(("t" "Personal Task"
        (file+headline org-default-notes-file "Tasks")
        "* TODO %?\n  %u\n  %a" :empty-lines 1)
       ("w" "Work-related Task" entry
        (file+headline org-default-notes-file "Work")
        "* TODO %?\n  %u" :empty-lines 1)
       ("g" "game idea" entry
        (file "~/notes/gameideas.org")
        "* %? %u")
       ("i" "random idea" entry
        (file "~/notes/ideas.org")
        "* %? %u")))

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
 '(custom-enabled-themes (quote (sanityinc-tomorrow-night)))
 '(custom-safe-themes
   (quote
    ("bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "e68cf26c0e2957f19cd8e857d9546b17a32ded41caa00a4f94c8276b2c19cb2b" "217ad4914762a1ff7dfb7dc55df85e37c11755fb2bb0c8a5df272e425bf669d9" "f87f74ecd2ff6dc433fb4af4e76d19342ea4c50e4cd6c265b712083609c9b567" default)))
 '(flycheck-typescript-tslint-config "tsconfig.json")
 '(flycheck-typescript-tslint-executable "/home/mrspeaker/.nvm/versions/node/v11.1.0/bin/tslint")
 '(hl-paren-colors
   (quote
    ("#B9F" "#B8D" "#B7B" "#B69" "#B57" "#B45" "#B33" "#B11")))
 '(neo-show-slash-for-folder nil)
 '(org-agenda-files (quote ("~/work.org")))
 '(package-selected-packages
   (quote
    (md4rd auto-dim-other-buffers all-the-icons-dired all-the-icons markdown-mode flycheck slack typescript-mode magit yasnippet-snippets yasnippet expand-region hackernews org-download url-http-ntlm js2-mode)))
 '(sunshine-appid "4a4924c8eb9b826d500afed6ea276dce")
 '(sunshine-location "11217,US")
 '(sunshine-show-icons t)
 '(sunshine-units (quote metric))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Liberation Mono" :foundry "1ASC" :slant normal :weight normal :height 120 :width normal))))
 '(auto-dim-other-buffers-face ((t (:background "#111"))))
 '(sunshine-forecast-headline-face ((t (:foreground "navajo white" :height 1.1)))))

; Line-wrap icons on right side only.
(setf (cdr (assq 'continuation fringe-indicator-alist))
			'(nil right-curly-arrow)
			)

; Menus and scrollbars
;(menu-bar-mode 1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(desktop-save-mode 1) ; Save layout/open files
(delete-selection-mode 1) ; Overwrite selection

(global-set-key (kbd "<mouse-3>") 'mouse-major-mode-menu)
(global-set-key (kbd "<C-mouse-3>") 'mouse-popup-menubar)

(require 'expand-region)
(global-set-key (kbd "C-'") 'er/expand-region)

(put 'narrow-to-region 'disabled nil) ; TODO: why?
(put 'dired-find-alternate-file 'disabled nil) ; TODO: why?

; Org-capture
(global-set-key (kbd "C-c c") 'org-capture)
(setq org-default-notes-file "~/notes/organizer.org")

;; Move lines up/down with M-p and M-n
(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(global-set-key (kbd "M-p")  'move-line-up)
(global-set-key (kbd "M-n")  'move-line-down)

; Reddit Mode
(setq md4rd-subs-active '(gamedev emacs orgmode spacex))

;(set-background-color "#1f1915")

; quiet, please! No dinging!
(setq visible-bell nil
      ring-bell-function 'flash-mode-line)
(defun flash-mode-line ()
  ;(invert-face 'mode-line)
  ;(run-with-timer 0.1 nil #'invert-face 'mode-line))
  (let ((orig-fg (face-foreground 'mode-line)))
          (set-face-foreground 'mode-line "#F2804F")
          (run-with-idle-timer 0.1 nil
                               (lambda (fg) (set-face-foreground 'mode-line fg))
                               orig-fg)))

; Repeatedly use C-spc after C-u spc to cycle through mark ring
(setq set-mark-command-repeat-pop t)
(global-set-key [remap upcase-region] 'ignore)  ; because it triggers whenever i typo c-x u
(global-set-key [remap org-cycle-agenda-files] 'ignore) ; tmp - same keybind as expand rgion above
; (global-set-key [remap flyspell-autocorrect-previous-word] 'ignore)

;;; ; tmp - same keybind as expand rgion above
