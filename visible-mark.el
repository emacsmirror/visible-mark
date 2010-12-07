;;; visible-mark.el --- Make marks visible.

;;; Commentary:

;; This was hacked together by Jorgen Schäfer
;; And hacked again by Yann Hodique
;; Donated to the public domain. Use at your own risk.

;;; History:
;; 2008-02-21  MATSUYAMA Tomohiro <t.matsuyama.pub@gmail.com>
;;
;;      * visible-mark.el: Added function to inhibit trailing overlay.
;;
;; 2008-01-31  MATSUYAMA Tomohiro <t.matsuyama.pub@gmail.com>
;;
;;      * visible-mark.el: Create formal emacs lisp file from
;;        http://www.emacswiki.org/cgi-bin/wiki/VisibleMark.
;;        Yann Hodique and Jorgen Schäfer are original authors.
;;        Added function to make multiple marks visible.
;;

;;; Code:

(eval-when-compile
  (require 'cl))

(defgroup visible-mark nil
  "Show the position of your mark."
  :group 'convenience
  :prefix "visible-mark-")

(defface visible-mark-face
  `((((type tty) (class color))
     (:background "gray" :foreground "black"))
    (((type tty) (class mono))
     (:inverse-video t))
    (((class color) (background dark))
     (:background "gray" :foreground "black"))
    (((class color) (background light))
     (:background "grey80"))
    (t (:background "gray")))
  "Face for the mark."
  :group 'visible-mark)

(defvar visible-mark-overlays nil
  "The overlays used in this buffer.")
(make-variable-buffer-local 'visible-mark-overlays)

(defcustom visible-mark-inhibit-trailing-overlay t
  "If non-nil, inhibit trailing overlay with underline face."
  :group 'visible-mark
  :type 'boolean)

(defcustom visible-mark-max 1
  "A number of mark to be visible."
  :group 'visible-mark
  :type 'integer)

(defcustom visible-mark-faces nil
  "A list of mark faces."
  :group 'visible-mark
  :type '(repeat face))

(defcustom global-visible-mark-mode-exclude-alist nil
  "A list of buffer names to be excluded"
  :group 'visible-mark
  :type '(repeat regexp))
                  
(defun visible-mark-initialize-overlays ()
  (mapc
   (lambda (x)
     (when (eq 'visible-mark (overlay-get x 'category))
       (delete-overlay x)))
   (overlays-in (point-min) (point-max)))  
  (let (overlays)
    (dotimes (i visible-mark-max)
      (let ((overlay (make-overlay (point-min) (point-min))))
        (overlay-put overlay 'category 'visible-mark)
        (push overlay overlays)))
    (setq visible-mark-overlays (nreverse overlays))))

(defun visible-mark-find-overlay-at (pos)
  (let ((overlays (overlays-at pos))
        found)
    (while (and overlays (not found))
      (let ((overlay (car overlays)))
        (if (eq 'visible-mark (overlay-get overlay 'category))
            (setq found overlay)))
      (setq overlays (cdr overlays)))
    found))

(defun visible-mark-move-overlays ()
  "Move the overlay in `visible-mark-overlay' to a new position."
  (mapc (lambda (x) (move-overlay x 0 0))
        visible-mark-overlays)
  (let ((marks (cons (mark-marker) mark-ring))
        (overlays visible-mark-overlays))
    (dotimes (i visible-mark-max)
      (let* ((mark (car-safe marks))
             (overlay (car overlays))
             (pos (and mark (marker-position mark))))
        (when pos
          (cond
            ((and visible-mark-inhibit-trailing-overlay
                  (save-excursion (goto-char pos) (eolp)))
             (overlay-put overlay 'face nil)
             (if (visible-mark-find-overlay-at pos)
                 (progn (overlay-put overlay 'before-string nil)
                        (move-overlay overlay 0 0))
                 (overlay-put overlay 'before-string
                              (propertize
                               " "
                               'face (or (nth i visible-mark-faces) 'visible-mark-face)
                               'cursor 0))
                 (move-overlay overlay pos (1+ pos))))
            (t
             (overlay-put overlay 'before-string nil)
             (overlay-put overlay 'face
                          (or (nth i visible-mark-faces) 'visible-mark-face))
             (move-overlay overlay pos (1+ pos)))))
        (setq marks (cdr marks)))
      (setq overlays (cdr overlays)))))

(require 'easy-mmode)

(defun visible-mark-mode-maybe ()
  (when (cond
         ((minibufferp (current-buffer)) nil)
         ((flet ((fun (arg)
                      (if (null arg) nil
                        (or (string-match (car arg) (buffer-name))
                            (fun (cdr arg))))))
            (fun global-visible-mark-mode-exclude-alist)) nil)
         (t t))
    (visible-mark-mode t)))

(define-minor-mode visible-mark-mode
  "A mode to make the mark visible."
  nil nil nil
  :group 'visible-mark
  (if visible-mark-mode
      (progn
        (visible-mark-initialize-overlays)
        (add-hook 'post-command-hook 'visible-mark-move-overlays nil t))
    (mapc 'delete-overlay visible-mark-overlays)
    (setq visible-mark-overlays nil)
    (remove-hook 'post-command-hook 'visible-mark-move-overlays t)))

(define-global-minor-mode
  global-visible-mark-mode visible-mark-mode visible-mark-mode-maybe
  :group 'visible-mark)

(provide 'visible-mark)
;;; visible-mark.el ends here
