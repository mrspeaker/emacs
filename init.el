(require 'package)
(setq package-enable-at-startup nil)

(setq package-archives '(("ELPA"  . "http://tromey.com/elpa/")
			 ("gnu"   . "http://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")
			 ("org"   . "https://orgmode.org/elpa/")))

(package-initialize)

;;; Bootstrapping use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

; Load the actual init.el from an org file
(when (file-readable-p "~/.emacs.d/config.org")
  (org-babel-load-file (expand-file-name "~/.emacs.d/config.org")))

; Custom vars not stored in config.
					; TODO: package loading etc should go to config

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
 '(beacon-color "#cc6666")
 '(company-backends
   (quote
    (company-tern company-tide company-bbdb company-eclim company-semantic company-clang company-xcode company-cmake company-capf company-files
                  (company-dabbrev-code company-gtags company-etags company-keywords)
                  company-oddmuse company-dabbrev)))
 '(css-indent-offset 2)
 '(custom-enabled-themes (quote (sanityinc-tomorrow-night)))
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
 '(md4rd-subs-active
   (quote
    (emacs gamedev spacex orgmode fortnitebr javascript)))
 '(neo-show-slash-for-folder nil)
 '(org-agenda-files (quote ("~/work.org")))
 '(org-startup-indented t)
 '(org-startup-with-inline-images t)
 '(package-selected-packages
   (quote
    (yasnippet-snippets use-package url-http-ntlm slack org-download markdown-mode magit gruvbox-theme expand-region company-tern auto-dim-other-buffers all-the-icons-dired)))
 '(pdf-view-midnight-colors (quote ("#fdf4c1" . "#1d2021")))
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
