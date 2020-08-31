(require 'all-the-icons)

(defvar all-the-icons--size-data
  (make-hash-table :test #'equal)
  "Icon size data (WIDTH . HEIGHT).  Keys formatted as (FAMILY . ICON-NAME)")

(defun segment-pixel-width (start end)
  "Calculate and return the width (in pixels) of the segment between START and END."
  (save-current-buffer
    (save-excursion
      (when (markerp start)
        (switch-to-buffer (marker-buffer start))
        (set-buffer (marker-buffer start)))
      (-let* ((start-x (window-x-pixel-position start))
              (end-x (window-x-pixel-position end)))
        (when (and start-x end-x)
          (- end-x start-x))))))

(defun segment-pixel-height (start end)
  "Calculate and return the height (in pixels) of the segment between START and END."
  (save-current-buffer
    (save-excursion
      (when (markerp start)
        (switch-to-buffer (marker-buffer start))
        (set-buffer (marker-buffer start)))
      (-let* ((start-y (window-y-pixel-position start))
              (end-y (window-y-pixel-position end)))
        (when (and start-y end-y)
          (- end-y start-y))))))

(defun window-x-pixel-position (pos)
  (car (progn (goto-char pos)
              (or (window-absolute-pixel-position pos)
                  (progn (redisplay)
                         (window-absolute-pixel-position pos))))))

(defun window-y-pixel-position (pos)
  (cdr (progn (goto-char pos)
              (or (window-absolute-pixel-position pos)
                  (progn (redisplay)
                         (window-absolute-pixel-position pos))))))

(defun build-icon-dimension-data ()
  "Build a database of rendered icon dimensions."
  (interactive)
  (save-current-buffer
    (let ((buffer (get-buffer-create "*icon-indexing*")))
      (switch-to-buffer buffer)
      (delete-region (point-min) (point-max))
      (let ((line-height (line-pixel-height)) ; 15
            (char-width (progn (insert " ") (segment-pixel-width (point-min) (point-max))))) ; 7
        (mapc
         (lambda (family)
           (let ((family-name (funcall (all-the-icons--family-name family)))
                 (data-alist (funcall (all-the-icons--data-name family))))
             (mapcar
              (-lambda ((name . icon))
                (delete-region (point-min) (point-max))
                (insert (propertize icon 'font-lock-ignore t 'face `(:family ,family-name :height 4.0)))
                (redisplay)
                (puthash (cons family name)
                         (cons (/ (segment-pixel-width (point-min) (point-max)) (* char-width 4.0))
                               (/ (line-pixel-height) (* line-height 4.0)))
                         all-the-icons--size-data))
              data-alist)))
         all-the-icons-font-families))
      (kill-buffer buffer))))

(build-icon-dimension-data)

(defmacro define-icon (name alist family &optional font-name)
  "Macro to generate functions for inserting icons for icon set NAME.

NAME defines is the name of the iconset and will produce a
function of the for `all-the-icons-NAME'.

ALIST is the alist containing maps between icon names and the
UniCode for the character.  All of these can be found in the data
directory of this package.

FAMILY is the font family to use for the icons.
FONT-NAME is the name of the .ttf file providing the font, defaults to FAMILY."
  `(progn
     (add-to-list 'all-the-icons-font-families (quote ,name))
     (add-to-list 'all-the-icons-font-names (quote ,(downcase (format "%s.ttf" (or font-name family)))))
     (defun ,(all-the-icons--family-name name) () ,family)
     (defun ,(all-the-icons--data-name name) () ,alist)
     (defun ,(all-the-icons--function-name name) (icon-name &rest args)
       (let* ((icon (cdr (assoc icon-name ,alist)))
              (other-face (when all-the-icons-color-icons (plist-get args :face)))
              (scale (car (gethash (cons ',name icon-name) all-the-icons--size-data)))
              (height (/ (if (> scale 1.2) 2 1) scale))
              (v-adjust (/ (* (if (> scale 1.2) 2 1) (or (plist-get args :v-adjust) all-the-icons-default-adjust)) scale))
              (family ,family))
         (unless icon
           (error (format "Unable to find icon with name `%s' in icon set `%s'" icon-name (quote ,name))))
         (let ((face (if other-face
                         `(:family ,family :height ,height :inherit ,other-face)
                       `(:family ,family :height ,height))))
           (propertize icon
                       'face face           ;so that this works without `font-lock-mode' enabled
                       'font-lock-face face ;so that `font-lock-mode' leaves this alone
                       'display `(raise ,v-adjust)
                       'rear-nonsticky t))))
     (defun ,(all-the-icons--insert-function-name name) (&optional arg)
       ,(format "Insert a %s icon at point." family)
       (interactive "P")
       (all-the-icons-insert arg (quote ,name)))))

(define-icon alltheicon all-the-icons-data/alltheicons-alist "all-the-icons")
(define-icon fileicon all-the-icons-data/file-icon-alist "file-icons")
(define-icon faicon all-the-icons-data/fa-icon-alist "FontAwesome")
(define-icon octicon all-the-icons-data/octicons-alist "github-octicons" "octicons")
(define-icon wicon all-the-icons-data/weather-icons-alist "Weather Icons" "weathericons")
(define-icon material all-the-icons-data/material-icons-alist "Material Icons" "material-design-icons")
