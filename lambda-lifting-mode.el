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

(defun lambda-lifting-check-conflict (beg end)
  (let ((result nil))
    (dolist (overlay (overlays-in beg end) result)
      (if (eq (overlay-get overlay 'type) 'lambda-lifting)
          (setq result t)))))

(defun lambda-lifting-genereta-spacing (width)
  (propertize " " 'display
              `(space . (:width (,width)))))

(defun lambda-lifting-handle-lifting (beg end repl)
  (when (not (lambda-lifting-check-conflict beg end))
    (let ((oldwidth (car (window-text-pixel-size nil beg end)))
          (overlay (make-overlay beg end)))
      (overlay-put overlay 'type 'lambda-lifting)
      (overlay-put overlay 'display repl)
      (let* ((newwidth (car (window-text-pixel-size nil beg end)))
             (remaining (- oldwidth newwidth))
             (before (/ remaining 2))
             (after (- remaining before)))
        (overlay-put overlay 'before-string (lambda-lifting-genereta-spacing before))
        (overlay-put overlay 'after-string (lambda-lifting-genereta-spacing after))))))

(defun lambda-lifting-fontify (beg end)
  (lambda-lifting-unfontify beg end)
  (dolist (subst lambda-lifting-substitutions)
    (save-excursion
      (goto-char beg)
      (let ((regex (car subst))
            (repl (cdr subst)))
        (while (re-search-forward regex end t)
          (lambda-lifting-handle-lifting (match-beginning 1) (match-end 1) repl))))))

(defun lambda-lifting-unfontify (beg end)
  (dolist (overlay (overlays-in beg end))
    (if (eq (overlay-get overlay 'type) 'lambda-lifting)
        (delete-overlay overlay))))

(defun lambda-lifting-clear ()
  (lambda-lifting-unfontify (point-min) (point-max)))

(define-minor-mode lambda-lifting-mode
  ""
  nil " λ" nil
  (if lambda-lifting-mode
      (jit-lock-register 'lambda-lifting-fontify)
    (jit-lock-unregister 'lambda-lifting-fontify)
    (lambda-lifting-clear)))

(provide 'lambda-lifting-mode)
