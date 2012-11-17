;; simple rules:
;;  - all vars, fns, macros: my-<name>
;;  - remember the rule (dee hock):
;;      Simple, clear purpose and principles give rise
;;      to complex and intelligent behavior. Complex rules
;;      and regulations give rise to simple, stupid behavior.
;;  - notice big picture; notice big patterns and
;;    build helpful tools for them
;;  - and most importantly, don't fight the system!

(defun my-load (path)
  (let* ((path (expand-file-name path)) ;binds w/ default-directory
         (src (concat path ".el"))
         (cpl (concat path ".elc")))
    (if (file-newer-than-file-p src cpl)
        (byte-compile-file src))
    (load cpl
          nil nil t)))

(defvar my-dot-emacs-home
  (expand-file-name "emacs"
                    "~/code/mylib"))

(add-to-list 'load-path
             (expand-file-name "ruby"
                               my-dot-emacs-home))

(let ((default-directory my-dot-emacs-home))
  (add-to-list 'load-path
               (expand-file-name "cust"))
  
  (my-load "key-bindings")
  (my-load "default")

  (if window-system
      (my-load "x-win"))
  )

(garbage-collect)
(message "")
