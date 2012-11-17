(require 'message)

(define-key message-mode-map "\C-c\C-c"
  '%%message-send-and-exit)

(defvar %%message-send-and-exit-count 0)

(let ((default-directory "~/.mesg-queue/"))
  (setq %%message-send-and-exit-count
        (- (length (directory-files "")) 2)))

(defun %%message-send-and-exit (&optional arg)
  (interactive)
  (let ((default-directory "~/.mesg-queue/")
        (filename (prin1-to-string %%message-send-and-exit-count)))
    (write-region (point-min) (point-max)
                  filename nil nil nil 'must-be-new)
    (set-buffer-modified-p nil)
    (kill-buffer (current-buffer))
    (incf %%message-send-and-exit-count)))

(defun %%really-send-mail.old ()
  (interactive)
  ;; my modified SCP
  (let ((files (cddr (directory-files "~/.mesg-queue/"))))
    (when files
      (let ((buffer "*Sending Mail*"))
        (with-current-buffer (get-buffer-create buffer)
          (insert "\f\n\f\n"))
        (let ((proc
               (apply #'call-process
                      (expand-file-name "~/bin/scp") nil
                      buffer nil "scp" ".mesg-queue" files)))
          (unless (and (process-exit-status proc)
                       (zerop (process-exit-status proc)))
            (error "Couldn't run SCP correctly"))
          (mapcar #'delete-file
                  (cddr (directory-files "~/.mesg-queue/" t)))
          (start-process "ssh" buffer
                         (expand-file-name "~/bin/scp")
                         "ssh" "~/sendm/send"))))))

(defun %%really-send-mail ()
  (interactive)
  ;; my modified SCP
  (let ((files (cddr (directory-files "~/.mesg-queue/"))))
    (when files
      (let ((buffer "*Sending Mail*"))
        (with-current-buffer (get-buffer-create buffer)
          (insert "\f\n\f\n"))
        (with-current-buffer "*shell*"
          (goto-char (point-max))
          (insert "cd ~/.mesg-queue/; scp scp .mesg-queue *")
          (comint-send-input)
          (insert "scp ssh \"~/sendm/send\"\n")
          (comint-send-input))))))
