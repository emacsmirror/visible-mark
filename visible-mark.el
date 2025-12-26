;;; visible-mark.el --- Make marks visible -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2014 by Ian Kelling

;; Maintainer: Campbell Barton <ideasman42@gmail.com>
;; Author: Ian Kelling <ian@iankelling.org>
;; Author: Yann Hodique
;; Author: MATSUYAMA Tomohiro <t.matsuyama.pub@gmail.com>
;; Author: John Foerch <jjfoerch@earthlink.net>

;; URL: https://codeberg.org/ideasman42/emacs-visible-mark
;; Keywords: marking color faces
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:

;; Emacs minor mode to highlight mark(s).
;;
;; Allows setting the number of marks to display, and the faces to display them.
;;
;; A good blog post was written introducing this package:
;; http://pragmaticemacs.com/emacs/regions-marks-and-visual-mark/
;;
;; Example installation:
;;
;; 1. Put this file in Emacs's load-path
;;
;; 2. add custom faces to init file
;; (require 'visible-mark)
;; (global-visible-mark-mode 1) ;; or add (visible-mark-mode) to specific hooks
;;
;; 3. Add customizations.  The defaults are very minimal.
;; They could also be set via customize.
;;
;; (defface visible-mark-active ;; put this before (require 'visible-mark)
;;   '((((type tty) (class mono)))
;;     (t (:background "magenta"))) "")
;; (setq visible-mark-max 2)
;; (setq visible-mark-faces `(visible-mark-face1 visible-mark-face2))
;;
;;
;; Additional useful functions like unpoping the mark are at
;; http://www.emacswiki.org/emacs/MarkCommands
;; and http://www.emacswiki.org/emacs/VisibleMark

;; Known bugs
;;
;; Observed in circe, when the buffer has a right margin, and there
;; is a mark at the beginning of a line, any text in the margin on that line
;; gets styled with the mark's face.  May also happen for left margins, but
;; haven't tested yet.
;;
;; Patches / pull requests / feedback welcome.

;;; Code:

(defgroup visible-mark nil
  "Show the position of your mark."
  :group 'convenience
  :prefix "visible-mark-")

(defface visible-mark-active
  '((((type tty) (class color)) (:background "gray" :foreground "black"))
    (((type tty) (class mono)) (:inverse-video t))
    (((class color) (background dark)) (:background "gray" :foreground "black"))
    (((class color) (background light)) (:background "grey80"))
    (t (:background "gray")))
  "Face for the active mark.
To redefine this in your init file,
do it before loading/requiring visible-mark."
  :group 'visible-mark)

(defcustom visible-mark-inhibit-trailing-overlay t
  "Inhibit the overlay from extending from the line-end to the window margin."
  :group 'visible-mark
  :type 'boolean)

;; Historic name (package-lint complains).
(define-obsolete-variable-alias
  'global-visible-mark-mode-exclude-alist
  'visible-mark-mode-global-exclude
  "0.1.0")

(defcustom visible-mark-mode-global-exclude nil
  "A list of buffer names to be excluded."
  :group 'visible-mark
  :type '(repeat regexp))

(defcustom visible-mark-max 1
  "The number of marks in the backward direction to be visible."
  :group 'visible-mark
  :type 'integer)

(defcustom visible-mark-forward-max 0
  "The number of marks in the forward direction to be visible."
  :group 'visible-mark
  :type 'integer)

(defcustom visible-mark-faces nil
  "A list of mark faces for marks in the backward direction.
If `visible-mark-max' is greater than the amount of `visible-mark-faces',
the last defined face will be reused."
  :group 'visible-mark
  :type '(repeat face))

(defcustom visible-mark-forward-faces nil
  "A list of mark faces for marks in the forward direction."
  :group 'visible-mark
  :type '(repeat face))


;;; example faces

(defface visible-mark-face1
  '((((type tty) (class mono))) (t (:background "light salmon")))
  "Example face which can be customized and added to subsequent face lists."
  :group 'visible-mark)

(defface visible-mark-face2
  '((((type tty) (class mono))) (t (:background "light goldenrod")))
  "Example face which can be customized and added to subsequent face lists."
  :group 'visible-mark)

(defface visible-mark-forward-face1
  '((((type tty) (class mono))) (t (:background "pale green")))
  "Example face which can be customized and added to subsequent face lists."
  :group 'visible-mark)

(defface visible-mark-forward-face2 nil
  "Placeholder face for customization and addition to subsequent face lists."
  :group 'visible-mark)

;; Make local on buffer activation.
(defvar visible-mark--overlays nil
  "The overlays used for mark faces.  Used internally by `visible-mark-mode'.")

(defun visible-mark--initialize-overlays ()
  "Setup overlays in the current buffer."
  (mapc
   (lambda (x)
     (when (eq 'visible-mark (overlay-get x 'category))
       (delete-overlay x)))
   (overlays-in (point-min) (point-max)))
  (let (overlays)
    (dotimes (_ (+ visible-mark-max visible-mark-forward-max))
      (let ((overlay (make-overlay (point-min) (point-min))))
        (overlay-put overlay 'category 'visible-mark)
        (push overlay overlays)))
    (setq visible-mark--overlays (nreverse overlays))))

(defun visible-mark--find-overlay-at (pos)
  "Return the visible-mark overlay at POS."
  (let ((overlays (overlays-at pos))
        found)
    (while overlays
      (let ((overlay (car overlays)))
        (setq overlays (cdr overlays))
        (when (eq 'visible-mark (overlay-get overlay 'category))
          (setq overlays nil) ; Break.
          (setq found overlay))))
    found))

(defun visible-mark--move-overlays ()
  "Update overlays in `visible-mark--overlays'.
This is run in the `post-command-hook'."
  (mapc (lambda (x) (delete-overlay x)) visible-mark--overlays)
  (let ((marks (cons (mark-marker) mark-ring))
        (overlays visible-mark--overlays)
        (faces visible-mark-faces)
        (faces-forward visible-mark-forward-faces))
    (when mark-active
      (setq faces (cons 'visible-mark-active (cdr faces))))
    (dotimes (_ visible-mark-max)
      (visible-mark--move-overlay (pop overlays) (pop marks) (car faces))
      (when (cdr faces)
        (pop faces)))
    (dotimes (i visible-mark-forward-max)
      (visible-mark--move-overlay
       (pop overlays) (car (last marks (1+ i))) (car faces-forward))
      (when (cdr faces-forward)
        (pop faces-forward)))))

(defun visible-mark--move-overlay (overlay mark face)
  "Set OVERLAY to position of MARK and display of FACE."
  (let ((pos (and mark (marker-position mark))))
    (when (and pos (null (equal (point) pos)))
      (let ((pos-end (min (1+ pos) (point-max))))
        (cond
         ((and visible-mark-inhibit-trailing-overlay
               (save-excursion
                 (goto-char pos)
                 (eolp)))
          (overlay-put overlay 'face nil)
          (cond
           ((visible-mark--find-overlay-at pos)
            (overlay-put overlay 'before-string nil))
           (t
            (overlay-put overlay 'before-string (propertize " " 'face face))
            (move-overlay overlay pos pos-end))))
         (t
          (overlay-put overlay 'before-string nil)
          (overlay-put overlay 'face face)
          (move-overlay overlay pos pos-end)))))))

(defun visible-mark--mode-maybe ()
  "Enable visible mark mode based on the context."
  (when (cond
         ((minibufferp (current-buffer))
          nil)
         ((let ((name (buffer-name))
                (exclusions visible-mark-mode-global-exclude)
                (found nil))
            (while exclusions
              (let ((arg (car exclusions)))
                (setq exclusions (cdr exclusions))
                (when (string-match-p arg name)
                  (setq exclusions nil) ; Break.
                  (setq found t))))
            found)
          nil)
         (t
          t))
    (visible-mark-mode t)))

;;;###autoload
(define-minor-mode visible-mark-mode
  "A mode to make the mark visible."
  :global nil

  (cond
   (visible-mark-mode
    (make-local-variable 'visible-mark--overlays)
    (visible-mark--initialize-overlays)
    (add-hook 'post-command-hook #'visible-mark--move-overlays nil t))
   (t
    (mapc 'delete-overlay visible-mark--overlays)
    (setq visible-mark--overlays nil)
    (remove-hook 'post-command-hook #'visible-mark--move-overlays t)
    (kill-local-variable 'visible-mark--overlays))))

;;;###autoload
(define-globalized-minor-mode global-visible-mark-mode
  visible-mark-mode
  visible-mark--mode-maybe)

(provide 'visible-mark)
;;; visible-mark.el ends here
