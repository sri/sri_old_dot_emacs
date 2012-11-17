;;; Window

(global-font-lock-mode -1)

(setq frame-title-format " ")
;(multiple-frames "%b" ("" invocation-name "@" system-name))
(setq baud-rate (expt 2 24))
  
(when (and interprogram-cut-function interprogram-paste-function)
  (setq interprogram-cut-function
        (lambda (text &optional push)
          (when (listp last-nonmenu-event)
            (x-select-text text push))))
  (setq interprogram-paste-function
        (lambda ()
          (when (listp last-nonmenu-event)
            (x-cut-buffer-or-selection-value)))))

;;; Frames
(defvar default-font
  "-b&h-lucidatypewriter-medium-r-normal-sans-12-120-75-75-m-70-iso8859-1"
  "Name of the default font.")


(let ((common `((border-color     . "White")
                (foreground-color . "Gray")
                (background-color . "Black")
                (background-mode  . light)
                (mouse-color      . "Yellow")
                (cursor-color     . "Yellow")
                (tool-bar-lines   . 0)
                (font             . ,default-font)
                (vertical-scroll-bars))))
  (setq default-frame-alist common))

(let* ((name (or (cdr (assoc 'name initial-frame-alist))
                 ""))
       (common '((minibuffer . t) (left . 805)
                 ;; (height . 60) (width . 113)
                 (cursor-color . "grey1") (cursor-type . (bar . 1))))
       (display-0 `((top . 0) ,@common))
       (display-1 `((top . 397) ,@common))
       (display-2 `((top . 800) ,@common)))
  ;(setq special-display-buffer-names
  ;  `(("*Shell Command Output*" (name . ,(format "%s -- %s" "output" name))
  ;     ,@display-0)
  ;    ("*Completions*" (name . ,(format "%s -- %s" "choices" name))
  ;     ,@display-0)))
    
    ;        ("*Async Shell Command*" (name . ,(format "%s -- %s" "process" name))  ,@display-0)
    ;        ("*Help*" (name . ,(format "%s -- %s" "help" name)) ,@display-0)))
    )

(push '(menu-bar-lines . 0) special-display-frame-alist)
(push '(border-width . 2) special-display-frame-alist)

;; Now locally bind a key that will easily delete the frame after
;; you are done with it.
;(dolist (x special-display-frame-alist)
;      (let ((buffer-name (car x)))
