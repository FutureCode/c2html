(setq get-props-function
      (lambda ()
        (let ((props nil))
          (let* ((i 0)
                 (end (- (point-max) (point-min)))
                 ;; initialize `prop' as the first character
                 ;; `prop' is in the form (FACE BEGIN END)
                 (current-face (get-text-property (+ i (point-min)) 'face))
                 (prop (list current-face i i)))
            (while (not (symbolp current-face))
              (setq current-face (car current-face)))
            (if (eq current-face 'font-lock-comment-delimiter-face)
                (setq current-face 'font-lock-comment-face))
            
            (while (< i end)
              (setq current-face (get-text-property (+ i (point-min)) 'face))
              (while (not (symbolp current-face))
                (setq current-face (car current-face)))
              (if (equal current-face (car prop))
                  (incf (nth 2 prop))
                (progn
                  (setq props (append props (list prop)))
                  (setq prop (list current-face i (1+ i)))))
              (incf i))
            (setq props (append props (list prop))))
          props )))

(setq get-face-list-function
      (lambda (props)
        (let ((r nil))
          (dolist (i props)
            (let ((current-face (car i)))
              (if (eq current-face 'font-lock-comment-delimiter-face)
                  (setq current-face 'font-lock-comment-face))
              (if (not (eq current-face nil))
                  (add-to-list 'r current-face))))
          r )))

(defun code-to-html-css ()
  "Convert a font-lock buffer to HTML with CSS"
  (interactive)
  ;; 1. gather information about the style of text
  ;; and store them in `props', in the form (prop1 prop2 ...)
  (let* ((props (funcall get-props-function))
         (face-list (funcall get-face-list-function props))
         (str-to-color (lambda (str)
                         (let ((r "#")
                               (l (color-values str)))
                           (dolist (channel l)
                             (setq r (concat r (format "%02X" (/ channel 256)))))
                           r ))))
    (let ((major-mode-name (prin1 major-mode))
          (possible-file-name (buffer-name)))
      ;; 2. output the HTML file
      (setq whole-buffer (buffer-substring-no-properties (point-min) (point-max)))
      (switch-to-buffer (concat possible-file-name ".html"))
      (insert "<html><head><link type=\"text/css\" rel=\"stylesheet\" href=\"a.css\"></head><body>\n")
      (insert (format "<div class=\"%s\">\n<p>\n" major-mode-name))
      (dolist (prop props)
        (let ((string (substring whole-buffer (nth 1 prop) (nth 2 prop))))
          (setq string (replace-regexp-in-string "&" "&amp;" string))
          (setq string (replace-regexp-in-string "<" "&lt;" string))
          (setq string (replace-regexp-in-string ">" "&gt;" string))
          (setq string (replace-regexp-in-string " " "&nbsp;" string))
          (setq string (replace-regexp-in-string "\n" "<br />\n" string))
          (insert (format "<span class=\"%S\">" (car prop)))
          (insert string)
          (insert "</span>")))
      (insert "</p></div></body></html>\n")
      ;; 3. output the CSS file
      (split-window-vertically)
      (other-window 1)
      (switch-to-buffer (concat possible-file-name ".css"))
      (insert (format ".%s {\n" major-mode-name))
      (insert (format "color: %s;\n"
                      (funcall str-to-color
                               (frame-parameter nil 'foreground-color))))
      (insert (format "background-color: %s;\n"
                      (funcall str-to-color
                               (frame-parameter nil 'background-color))))
      (insert "font-family: monospace;\n")
      (insert "}\n")
      (dolist (face face-list)
        (insert (format ".%s .%S {\n" major-mode-name face))
        (if (stringp (face-foreground face))
            (insert (format "color: %s;\n"
                            (funcall str-to-color
                                     (face-foreground face)))))
        (insert "}\n")))))

(defun code-to-html-inline ()
  "Convert a font-lock buffer to HTML with inline style"
  (interactive)
  (let* ((props (funcall get-props-function))
         (face-list (funcall get-face-list-function props))
         (face-prop-table (make-hash-table :test 'equal))
         (str-to-color (lambda (str)
                         (let ((r "#")
                               (l (color-values str)))
                           (dolist (channel l)
                             (setq r (concat r (format "%02X" (/ channel 256)))))
                           r ))))
    (dolist (face face-list)
      (puthash face
               (format "color: %s;" (funcall str-to-color
                                             (face-attribute face :foreground nil t)))
               face-prop-table))
    (setq whole-buffer (buffer-substring-no-properties (point-min) (point-max)))
    (let ((possible-file-name (buffer-name)))
      (switch-to-buffer (concat possible-file-name ".html"))
      (insert "<table><tr>\n")
      (insert (format "<td bgcolor=\"%s\">\n"
                      (funcall str-to-color
                               (frame-parameter nil 'background-color))))
      (insert (format "<span style=\"color: %s; font-family: monospace;"
                      (funcall str-to-color
                               (frame-parameter nil 'foreground-color))))
      (insert " font-size: 10.0pt;\">")
      (dolist (prop props)
        (let ((string (substring whole-buffer (nth 1 prop) (nth 2 prop))))
          (setq string (replace-regexp-in-string "&" "&amp;" string))
          (setq string (replace-regexp-in-string "<" "&lt;" string))
          (setq string (replace-regexp-in-string ">" "&gt;" string))
          (setq string (replace-regexp-in-string " " "&nbsp;" string))
          (setq string (replace-regexp-in-string "\n" "<br />\n" string))
          (if (eq nil (gethash (car prop) face-prop-table))
              (insert "<span>")
            (insert (format "<span style=\"%s\">" (gethash (car prop) face-prop-table))))
          (insert string)
          (insert "</span>")))
      (insert "\n</span></td></tr>\n</table>"))))

(provide 'c2html)

