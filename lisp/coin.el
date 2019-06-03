(interactive)
  (let ((site "https://coinalyze.net/btcusd-live-price-chart-bitstamp/")
        (file (find-file "~/tmp-coin")))
    ;; Add date and time
    (switch-to-buffer file)
    (goto-char (point-min))
    (insert (format-time-string "%F %T %Z" nil t))
    (newline 1)

    ;; Give eww some time to load
    (eww site)
    (sit-for 5)

    (re-search-forward "btc / usd")

    (end-of-visual-line)
    
    (set-mark-command nil)

    (re-search-forward "book")
    (beginning-of-visual-line)

    (kill-ring-save t t t)
    (deactivate-mark)
    
    (switch-to-buffer file)
    (yank)
    (execute-kbd-macro [?\C-p backspace ?  ?\C-a backspace ?  ?| ?  ?\C-a backspace ?  ?| ?  ?\C-a backspace ?  ?| ?  ?\C-a backspace ?  ?| ?  ?\C-a ?\C-p backspace ?  ?| backspace ?\C-a backspace ?  ?| ?  ?\C-a backspace ?  ?\C-a backspace ?| ?  ?\C-a backspace ?\C-a])
    (save-buffer file)
    )


    ;; ;; Jump to "Home Page" header
    ;; (re-search-forward "^home page$")

    ;; ;; Stories look like this in eww:
    ;; ;;   <topic -- LINK>
    ;; ;;   <story>
    ;; ;;
    ;; ;;   <headline>

    ;; (dotimes (_ headline-count)
    ;;   ;; Navigate to headline
    ;;   (shr-next-link)
    ;;   (dotimes (_ 3)
    ;;     (forward-line))

    ;;   ;; Copy headline
    ;;   (set-mark-command nil)
    ;;   (move-end-of-line nil)
    ;;   (kill-ring-save t t t)
    ;;   (deactivate-mark)

    ;;   ;; Paste headline
    ;;   (switch-to-buffer file)
    ;;   (yank)
    ;;   (newline)
    ;;   (switch-to-buffer "*eww*"))

    ;; ;; Save and prepare file for next invocation
    ;; (switch-to-buffer file)
    ;; (newline 2)
    ;; (save-buffer file)))
