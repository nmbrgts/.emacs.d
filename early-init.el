;;; startup tweaks

;; start maximized
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; minimal splash
(setq fancy-startup-text '()
      fancy-about-text '()
      fancy-splash-image "~/.tyler-emacs/splash.png")

(defun fancy-startup-tail (&optional concise)
  nil)

(defun fancy-splash-head ()
  "Insert the head part of the splash screen into the current buffer."
  (let* ((image-file (fancy-splash-image-file))
	 (img (create-image image-file))
	 (image-width (and img (car (image-size img))))
	 (window-width (window-width))
         (adjust-left 3))
    (when img
      (insert (s-repeat 10 "\n"))
      (insert (propertize " " 'display
                          `(space :align-to (+ ,(- (/ window-width 2)
                                                   adjust-left)
                                               (-0.5 . ,img)))))
      (insert-image img)
      (setq-local cursor-type nil
                  global-hl-line-mode nil))))
