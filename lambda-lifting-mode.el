;;; lambda-lifting-mode.el --- Nice and correctly spaced Haskell operators.

;; Copyright (C) 2014 Stefan Haller

;; Author: Stefan Haller <haliner@gmail.com>
;; Version: 0.1
;; Created: 27 Aug 2014
;; Keywords: font-lock compositing Haskell
;; URL: http://www.github.com/haliner/lambda-lifting

;; License:
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Code:

(defvar lambda-lifting-substitutions
  '(("\\(->\\)"              . "→")
    ("\\(=>\\)"              . "⇒")
    ("\\(==\\)"              . "≡")
    ("\\(/=\\)"              . "≠")
    ("\\(=<\\)"              . "≤")
    ("\\(>=\\)"              . "≥")
    ("\\(::\\)"              . "∷")
    ("\\(||\\)"              . "∨")
    ("\\(&&\\)"              . "∧")
    ("\\(|\\) "              . "│")
    ("\\(()\\)"              . "∅")
    (" \\(\\.\\) "           . "·")
    ("\\(<\\*>\\)"           . "⊛")
    ("\\(<$>\\)"             . "⊕")
    ("\\(/\\)"               . "÷")
    ("\\<\\(not\\)\\>"       . "¬")
    ("\\(\\*\\)"             . "×")
    ("\\<\\(undefined\\)\\>" . "⊥")
    ("\\(\\\\\\).*?->"       . "λ")
    ("\\<\\(forall\\)\\>"    . "∀")))

;;;; Utilities

(defun lambda-lifting-genereta-spacing (width)
  (propertize " " 'display
              `(space . (:width (,width)))))

(defun lambda-lifting-determine-pixel-width (beg end)
  (car (window-text-pixel-size nil beg end)))


;;;; Overlay Handling

(defun lambda-lifting-overlay-p (overlay)
  (and (overlayp overlay)
       (eq (overlay-get overlay 'type) 'lambda-lifting)))

(defun lambda-lifting-check-conflict (beg end)
  (let ((result nil))
    (dolist (overlay (overlays-in beg end) result)
      (when (lambda-lifting-overlay-p overlay)
        (setq result t)))))

(defun lambda-lifting-make-overlay (beg end repl)
  (let* ((overlay (make-overlay beg end nil t nil))
         (width (lambda-lifting-determine-pixel-width beg end)))
    (overlay-put overlay 'type 'lambda-lifting)
    (overlay-put overlay 'lambda-lifting `((repl . ,repl) (width . ,width)))
    overlay))

(defun lambda-lifting-activate-overlay (overlay)
  (when (lambda-lifting-overlay-p overlay)
    (let* ((alist (overlay-get overlay 'lambda-lifting))
           (repl (cdr (assoc 'repl alist)))
           (oldwidth (cdr (assoc 'width alist))))
      (overlay-put overlay 'display repl)
      (let* ((newwidth (lambda-lifting-determine-overlay-pixel-width overlay))
             (remaining (- oldwidth newwidth))
             (before (/ remaining 2))
             (after (- remaining before)))
        (overlay-put overlay 'before-string (lambda-lifting-genereta-spacing before))
        (overlay-put overlay 'after-string (lambda-lifting-genereta-spacing after))))))

(defun lambda-lifting-deactivate-overlay (overlay)
  (when (lambda-lifting-overlay-p overlay)
    (overlay-put overlay 'display nil)
    (overlay-put overlay 'before-string nil)
    (overlay-put overlay 'after-string nil)))


;;;; Text Properties

(defun lambda-lifting

(defun lambda-lifting-put-text-property (beg end repl)
  (let* ((width (lambda-lifting-determine-pixel-width beg end))
         (alist `((width . ,width) (repl . ,repl))))
    (put-text-property beg end 'lambda-mode alist)))

(defun lambda-lifting-remove-text-property (beg end)
  (remove-list-of-text-properties beg end '(lambda-mode)))


;;;; Fontification

(defun lambda-lifting-handle-lifting (beg end repl)
  (when (not (lambda-lifting-check-conflict beg end))
    (lambda-lifting-activate-overlay (lambda-lifting-make-overlay beg end repl))))

(defun lambda-lifting-fontify (beg end)
  (lambda-lifting-unfontify beg end)
  (dolist (subst lambda-lifting-substitutions)
    (save-excursion
      (goto-char beg)
      (let ((regex (car subst))
            (repl (cdr subst))
            (case-fold-search nil))
        (while (re-search-forward regex end t)
          (lambda-lifting-handle-lifting (match-beginning 1) (match-end 1) repl))))))

(defun lambda-lifting-unfontify (beg end)
  (dolist (overlay (overlays-in beg end))
    (when (lambda-lifting-overlay-p overlay)
      (delete-overlay overlay))))

(defun lambda-lifting-clear ()
  (lambda-lifting-unfontify (point-min) (point-max)))


;;;; Minor mode specifications

(defun lambda-lifting-initialize ()
  (add-to-list 'font-lock-extra-managed-props 'lambda-lifting)
  (add-to-list 'text-property-default-nonsticky '(lambda-lifting . 't))
  (jit-lock-register 'lambda-lifting-fontify))

(defun lambda-lifting-finalize ()
  (jit-lock-unregister 'lambda-lifting-fontify)
  (lambda-lifting-clear)
  (setq font-lock-extra-managed-props (delete 'lambda-lifting font-lock-extra-managed-props)))

;;;###autoload
(define-minor-mode lambda-lifting-mode
  ""
  nil " λ" nil
  (if lambda-lifting-mode
      (lambda-lifting-initialize)
    (lambda-lifting-finalize)))

(provide 'lambda-lifting-mode)

;;; lambda-lifting-mode.el ends here
