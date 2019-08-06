;;; init.el --- Emacs init file
;;  Author: Mr Speaker
;;; Commentary:
;;  My very own Emacs config file
;;; Code:
(require 'package)
(setq package-enable-at-startup nil)

(setq package-archives '(("ELPA"  . "http://tromey.com/elpa/")
			 ("gnu"   . "http://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")
			 ("org"   . "https://orgmode.org/elpa/")))

(package-initialize)

(setenv "PATH" (concat "/usr/local/bin:"
                       (getenv "PATH")))

(setq shr-color-visible-luminance-min 70) ; nicer eww background
(global-auto-revert-mode) ;; testing this

;;; Bootstrapping use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

; Load the actual init.el from an org file
(when (file-readable-p "~/.emacs.d/config.org")
  (org-babel-load-file (expand-file-name "~/.emacs.d/config.org")))

; Custom vars not stored in config.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file :noerror))

;;; init.el ends here
