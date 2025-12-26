;;; visible-mark.el --- Highlight marks in buffers -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2014 Ian Kelling

;; Maintainer: Campbell Barton <ideasman42@gmail.com>
;; Author: Ian Kelling <ian@iankelling.org>
;; Author: Yann Hodique
;; Author: MATSUYAMA Tomohiro <t.matsuyama.pub@gmail.com>
;; Author: John Foerch <jjfoerch@earthlink.net>

;; URL: https://codeberg.org/ideasman42/emacs-visible-mark
;; Keywords: convenience faces
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:

;; Minor mode to highlight mark(s).
;;
;; Allows setting the number of marks to display, and the faces to display them.
;;
;; A good blog post was written introducing this package:
;; https://pragmaticemacs.com/emacs/regions-marks-and-visual-mark/
;;
;; Example installation:
;;
;; 1. Put this file in Emacs's load-path
;;
;; 2. Add to your init file
;; (require 'visible-mark)
;; (global-visible-mark-mode 1) ;; or add (visible-mark-mode) to specific hooks
;;
;; 3. Add customizations.  The defaults are very minimal.
;; These can also be set via customize.
;;
;; (defface visible-mark-active ;; put this before (require 'visible-mark)
;;   '((((type tty) (class mono)) (:inverse-video t))
;;     (t (:background "magenta")))
;;   "Custom face for the active mark.")
;; (setq visible-mark-max 2)
;; (setq visible-mark-faces '(visible-mark-face1 visible-mark-face2))
;;
;; Additional useful functions like unpopping the mark are at
;; https://www.emacswiki.org/emacs/MarkCommands
;; and https://www.emacswiki.org/emacs/VisibleMark

;;; Known Bugs
;;
;; Observed in Circe, when the buffer has a right margin, and there
;; is a mark at the beginning of a line, any text in the margin on that line
;; gets styled with the mark's face.  This may also occur with left margins,
;; though this has not been verified.
;;
;; Patches/pull requests/feedback welcome.

;;; Code:

(require 'seq)

(defgroup visible-mark nil
  "Show the position of the mark(s)."
  :group 'convenience
  :prefix "visible-mark-")

(defface visible-mark-active
  '((((type tty) (class color)) (:background "gray" :foreground "black"))
    (((type tty) (class mono)) (:inverse-video t))
    (((class color) (background dark)) (:background "gray" :foreground "black"))
    (((class color) (background light)) (:background "grey80"))
    (t (:background "gray")))
  "Face for the active mark."
  :group 'visible-mark)

(defcustom visible-mark-inhibit-trailing-overlay t
  "When non-nil, prevent overlays from extending past line endings."
  :group 'visible-mark
  :type 'boolean)

;; Backward compatibility alias for the previous variable name.
(define-obsolete-variable-alias
  'global-visible-mark-mode-exclude-alist
  'visible-mark-mode-global-exclude
  "0.1.0")

(defcustom visible-mark-mode-global-exclude nil
  "A list of regexps matching buffer names to exclude.
This only applies to `global-visible-mark-mode'."
  :group 'visible-mark
  :type '(repeat regexp))

(defcustom visible-mark-max 1
  "The number of recent marks to display.
Marks are taken from the front of `mark-ring' (most recently set).
After changing this value, toggle `visible-mark-mode' off and on
for changes to take effect."
  :group 'visible-mark
  :type 'natnum)

(defcustom visible-mark-forward-max 0
  "The number of older marks to display.
Marks are taken from the end of `mark-ring' (oldest marks).
After changing this value, toggle `visible-mark-mode' off and on
for changes to take effect."
  :group 'visible-mark
  :type 'natnum)

(defcustom visible-mark-faces nil
  "A list of faces for recent marks.
When the mark is active, the first face is replaced with
`visible-mark-active' face.  If `visible-mark-max' is greater
than the number of faces, the last face is reused."
  :group 'visible-mark
  :type '(repeat face))

(defcustom visible-mark-forward-faces nil
  "A list of faces for older marks.
If `visible-mark-forward-max' is greater than the number of faces,
the last face is reused."
  :group 'visible-mark
  :type '(repeat face))


;;; Example Faces

(defface visible-mark-face1
  '((((type tty) (class mono)) (:inverse-video t))
    (t (:background "light salmon")))
  "Example face for use in `visible-mark-faces' or `visible-mark-forward-faces'."
  :group 'visible-mark)

(defface visible-mark-face2
  '((((type tty) (class mono)) (:inverse-video t))
    (t (:background "light goldenrod")))
  "Example face for use in `visible-mark-faces' or `visible-mark-forward-faces'."
  :group 'visible-mark)

(defface visible-mark-forward-face1
  '((((type tty) (class mono)) (:inverse-video t))
    (t (:background "pale green")))
  "Example face for use in `visible-mark-faces' or `visible-mark-forward-faces'."
  :group 'visible-mark)

(defface visible-mark-forward-face2 nil
  "Placeholder face for customization and addition to subsequent face lists."
  :group 'visible-mark)

;; Buffer-local storage for overlays; set when mode is enabled.
(defvar visible-mark--overlays nil
  "The overlays used to display mark faces.")

(defun visible-mark--initialize-overlays ()
  "Setup overlays in the current buffer."
  (mapc
   (lambda (overlay)
     (when (eq 'visible-mark (overlay-get overlay 'category))
       (delete-overlay overlay)))
   (overlays-in (point-min) (point-max)))
  (let (overlays)
    (dotimes (_ (+ visible-mark-max visible-mark-forward-max))
      (let ((overlay (make-overlay (point-min) (point-min))))
        (overlay-put overlay 'category 'visible-mark)
        (push overlay overlays)))
    (setq visible-mark--overlays (nreverse overlays))))

(defun visible-mark--find-overlay-at (pos)
  "Return the visible-mark overlay at POS."
  (seq-find
   (lambda (overlay)
     (eq 'visible-mark (overlay-get overlay 'category)))
   (overlays-at pos)))

(defun visible-mark--move-overlays ()
  "Update overlays in `visible-mark--overlays'."
  (mapc #'delete-overlay visible-mark--overlays)
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
    ;; Precompute forward marks to avoid O(n²) from repeated `last' calls.
    ;; Use `reverse' (not `nreverse') since `last' returns a tail of `marks'.
    ;; Note: "forward" here refers to older marks in the ring, not buffer position.
    (let ((forward-marks (reverse (last marks visible-mark-forward-max))))
      (dotimes (_ visible-mark-forward-max)
        (visible-mark--move-overlay
         (pop overlays) (pop forward-marks) (car faces-forward))
        (when (cdr faces-forward)
          (pop faces-forward))))))

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
  "Enable `visible-mark-mode' unless buffer is excluded."
  (unless (or (minibufferp)
              (seq-some
               (lambda (pattern)
                 (string-match-p pattern (buffer-name)))
               visible-mark-mode-global-exclude))
    (visible-mark-mode 1)))

;;;###autoload
(define-minor-mode visible-mark-mode
  "Minor mode to highlight the mark in the buffer."
  :global nil

  (cond
   (visible-mark-mode
    (make-local-variable 'visible-mark--overlays)
    (visible-mark--initialize-overlays)
    (add-hook 'post-command-hook #'visible-mark--move-overlays nil t))
   (t
    (mapc #'delete-overlay visible-mark--overlays)
    (setq visible-mark--overlays nil)
    (remove-hook 'post-command-hook #'visible-mark--move-overlays t)
    (kill-local-variable 'visible-mark--overlays))))

;;;###autoload
(define-globalized-minor-mode global-visible-mark-mode
  visible-mark-mode
  visible-mark--mode-maybe)

(provide 'visible-mark)
;;; visible-mark.el ends here
