;;; Dired customizations

(define-key ctl-x-4-map (kbd "C-j")
  'dired-jump-other-window)

(define-key global-map (kbd "C-x d")
  'dired)

(defvar ctl-x-ctl-d-map (make-keymap)
  "Keymap for C-x C-d; for directory operations.")

(define-key ctl-x-map (kbd "C-d") ctl-x-ctl-d-map)

(define-keys (ctl-x-ctl-d-map "C-d" ctl-x-map)
  "C-c"  byte-recompile-directory
  "C-a"  update-autoloads-from-directories
  "c"    cd
  "d"    delete-directory
  "l"    list-directory
  "p"    pwd
  "m"    make-directory)


(add-hook 'dired-load-hook
  (lambda ()
    (require 'dired-x)

    (setq dired-dwim-target t)

  (setq dired-listing-switches "-lFDh") ; b -- can't delete filenames with spaces
  ;; ask for each directory
  (setq dired-recursive-deletes t)
  ;;
  ;; others: '(chgrp chmod chown compress copy
  ;;                 delete hardlink move print
  ;;                 shell symlink uncompress)
  (setq dired-no-confirm '(byte-compile load))
  (setq dired-omit-extensions
        (remove ".pdf" dired-omit-extensions))

  (define-keys dired-mode-map
    ;; was C-o, which is other-window globally
    "C-c C-o"  dired-display-file
    "C-o"      other-window

    ;; `f' is for flagging & marking files;
    ;; setting it to NIL makes it a prefix command
     "f"        nil
     "f a"      dired-flag-auto-save-files
     "f b"      dired-flag-backup-files
     "f c"      dired-clean-directory
     "f d"      dired-mark-directories
     "f f"      dired-mark-files-containing-regexp
     "f g"      dired-flag-garbage-files
     "f r"      dired-flag-files-regexp
     "f s"      dired-mark-symlinks
     "f x"      dired-mark-executables

     [left]   dired-up-directory
     [right]  dired-advertised-find-file

     "b"      dired-beginning-of-buffer  ;was undefined
     "e"      dired-end-of-buffer        ;was dired-find-file 

     ;; was describe-mode, which for me is always
     ;; under C-x ? m
     "h"  dired-view-html-as-text
     
     ;;      "j"    ;was undefined

     "SPC"    dired-next-dirline         ;was dired-next-line

     "C-c C-a"   beginning-of-line

     ;; see below;  now with an arg replace without query
     "Q"  dired-replace-in-files
     
     ;; these two are swapped
     "A"   dired-find-alternate-file
     "a"   dired-do-search
     )

   ))

(add-hook 'dired-mode-hook
	  (lambda ()
	    ;; Buffer local variable -- cannot setq-default
	    ;; otherwise all buffers have "Omit" on modeline.
	    (setq dired-omit-files-p t)
            (setq dired-omit-size-limit nil)))

(defun dired-beginning-of-buffer ()
  "Move to the first file in dired buffer."
  (interactive)
  (goto-char (point-min))
  (dired-initial-position (buffer-name)))

(defun dired-end-of-buffer ()
  "Move to the last file in dired buffer."
  (interactive)
  (goto-char (point-max))
  (dired-previous-line 1))

(mapc (lambda (ignore)
        (push ignore debug-ignored-errors))
      '("^No file on this line$"
        "^Setting current directory: permission denied, .*"
        "^No more subdirectories$"))

(defun dired-replace-in-marked-files (from to)
  (interactive
   "sReplace in marked files (regexp): \nsReplace by: \n")
  (replace-in-files (list (cons from to))
                    (dired-get-marked-files)))

(defun dired-replace-in-files (arg)
  (interactive "P")
  (call-interactively
   (if arg 'dired-replace-in-marked-files 'dired-do-query-replace-regexp)))

;;; TODO:  combine below with HTML functions in functions.el

(defun dired-view-html-as-text (&optional arg)
  ;; Interface is like DIRED-DO-DELETE.  With a prefix arg
  ;; or if no files are marked, use the file that is next to
  ;; the point.  Otherwise, use files that are marked.
  "View marked files or current file as HTML."
  (interactive "P")
  (let ((files (dired-get-marked-files nil arg)))
    (dolist (file files)
      (let* ((name (concat "LYNX:" (file-name-nondirectory file)))
             (buffer (get-buffer-create name)))
        (with-current-buffer buffer
          (when (= 1 (point-max))
            (setq buffer-read-only nil)
            (erase-buffer))
          (buffer-disable-undo))
        (let ((proc (start-process name buffer "lynx" "-dump" file)))
          (set-process-sentinel proc
            'dired-view-html-as-text-process-sentinel)
          ;; If PROC finishes before we set its sentinel,
          ;; we need to explicit do it ourselves.
          (unless (eq (process-status proc) 'run)
            (dired-view-html-as-text-process-sentinel proc)))))))

(defun dired-view-html-as-text-process-sentinel (proc &optional event)
  (with-current-buffer (process-buffer proc)
    (goto-char (point-min))
    (set-buffer-modified-p nil)
    (view-mode)
    (pop-to-buffer (process-buffer proc))))
