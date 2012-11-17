;;; configuration.el ---

(defun make-directory-for (attribute description)
  (let* ((directory
          (expand-file-name attribute
            (expand-file-name "various/" dot-emacs-home)))
         (init-file (expand-file-name (concat attribute ".el") directory)))
    (unless (file-directory-p directory)
      (make-directory directory t))
    (if (file-exists-p init-file)
        (cload init-file)
      (write-region
       (format ";;; %s -- initialization for %s\n;;; %s\n"
               (file-name-nondirectory init-file) attribute
               description)
       nil init-file nil 'quiet)
      (message "New initialization file for %s in %s"
               attribute init-file))
    (push directory load-path)
    (push directory local-load-path)))


(defmacro define-configuration-rule (directory documentation test)
  `(when ,test
     (make-directory-for (symbol-name ',directory) ,documentation)))


;;; Rules:
(define-configuration-rule home
  "Initializations for home."
  (and (eq system-type 'gnu/linux)
       (string= (user-login-name) "sri")))

(define-configuration-rule school
  "Initializations for school."
  (and (eq system-type 'usg-unix-v)
       ))

;;; configuration.el ends here
