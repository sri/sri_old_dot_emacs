;;; like in `paren.el' but highlight starting
;;; and ending non-blank chars of the current line.

(defvar current-line-starting-pos -1)
(defvar current-line-overlays (cons nil nil))
(defvar current-line-highlight-overlay
  

(defun current-line-highlight ()
  (let (start end)
    (setq start (save-excursion (beginning-of-line) (point)))
    (unless (= start current-line-starting-pos)
      (when (car current-line-overlays)
        (delete-overlay (car current-line-overlays)))
      (when (cdr current-line-overlays)
        (delete-overlay (cdr current-line-overlays)))
      (setq current-line-starting-pos start)
      (setq start
            (save-excursion
              (goto-char start)
              (skip-chars-forward " \t")
              (point)))
      (setq end
            (save-excursion
              (goto-char (point-at-eol))
              (skip-chars-backward " \t")
              (point)))
      (when (< start end)
        (let ((o1 (make-overlay start end))
              (o2 (make-overlay (1- end) end)))
          (overlay-put o1 'face 'show-paren-match-face)
          (overlay-put o2 'face 'show-paren-match-face)
          (setcar current-line-overlays o1)
          (setcdr current-line-overlays o2))))))

(setq current-line-timer
  (run-with-idle-timer (/ 1.0 8) t 'current-line-highlight))
;;(cancel-timer current-line-timer)
;;(current-line-highlight)



;; also see rms's animate.el

(defun permute (x)
  (let* ((vector (coerce x 'vector))
         (length (length vector)))
    (dotimes (i length)
      (let ((elt-i (aref vector i))
            (random (random length)))
        (aset vector i (aref vector random))
        (aset vector random elt-i)))
    (coerce vector
      (let ((type (type-of x)))
        (cond ((eq type 'cons) 'list)
              (t type))))))

;; (COL . LINE)

(defun %animate-show-buffer ()
  (let ((positions '())
        (line 0))
    (goto-char (window-start))
    (while (not (= (point) (window-end)))
      (if (eolp)
          (incf line)
        (push (list (char-after) (current-column) line)
              positions))
      (forward-char 1))
    (setq positions (permute positions))
    (let ((next-line-add-newlines t)
          (buffer-undo-list t)
          (cursor-type nil))
    (with-temp-buffer
      (switch-to-buffer (current-buffer))
      (dolist (x positions)
        (goto-char (point-min))
        (dotimes (j (caddr x))
          (next-line 1))
        (move-to-column (cadr x) t)
        (insert-char (car x) 1)
        (unless (eolp)
          (delete-char 1))
        (sit-for 0.03))))))

(%animate-show-buffer)
  