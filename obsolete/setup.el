 (require 'cl)
(require 'bytecomp)



;;; TODO: modify CLOAD and CFILE so that if the file
;;; isn't absolute path, get if from DOT-EMACS-HOME

(defun cload (files &optional force-compile use-load-path)
  "Byte-compile (if necessary) and load each file in FILES.
Byte-compiles files if source is newer than byte-compiled one,
or if the byte-compiled one doesn't exist.
FORCE-COMPILE non-NIL always forces byte-compilation."

  (unless (listp files)
    (setq files (list files)))
  (setq files
        (mapcar (lambda (file)
                  (let ((name
                         (if (string-match "\\.el$" file)
                             file
                           (concat file ".el"))))
                    (expand-file-name name)))
                files))
  (dolist (file files t)
    (when (or force-compile
              (file-newer-than-file-p file (byte-compile-dest-file file)))
      (byte-compile-file file))
    (unless use-load-path
      (setq file (expand-file-name file)))
    ;; If FILE is an absolute name, LOAD loads that.  Otherwise, it
    ;; searches directories in LOAD-PATH for that file.
    (load (byte-compile-dest-file file) nil nil t)))

(defun cfile (files &optional force-compile)
  (unless (listp files)
    (setq files (list files)))
  (setq files
        (mapcar (lambda (file)
                  (let ((name
                         (if (string-match "\\.el$" file)
                             file
                           (concat file ".el"))))
                    (expand-file-name name)))
                files))
  (let ((all t))
    (dolist (file files all)
      (if (or force-compile
              (file-newer-than-file-p file (byte-compile-dest-file file)))
          (byte-compile-file file)
        (setq all nil)))))




(defvar dot-emacs-home (expand-file-name "emacs/" "~/code/mylib")
  "Directory where user's Emacs customizations resides.")

(defvar original-load-path (copy-list load-path)
  "Copy of the original LOAD-PATH
This is the value of LOAD-PATH after Emacs starts up,
before any user modifications.")

(defvar local-load-path nil)
;  (let* (;(custom (expand-file-name "custom" dot-emacs-home))
;         (subdirs
;          (directory custom
;            :absolute t
;            :test (lambda (x)
;                    (and (not (string= x "non-packages"))
;                         (file-directory-p x))))))
;    (list* dot-emacs-home custom subdirs))
;  "User's load path.")

(dolist (path local-load-path)
  (push path load-path))


;;; `setup.el' always needs to be byte-compiled.  Make sure we
;;; have the latest version next time around, when we leave emacs
;;; this time.

(add-hook 'kill-emacs
  (lambda ()
    (with-cwd dot-emacs-home (cfile "setup"))))

;;(require 'ut-time)


(let ((default-directory dot-emacs-home)
      (files
       '("default"
         "key-bindings"
         "dired-cus"
         "misc-cus"
         )))

  (cload files)

  (when window-system
    (cload "x-win"))
  ) ;LET


(garbage-collect)
(message "")
