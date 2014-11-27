(defvar lambda-lifting-substitutions
  '(("\\(->\\)"              . "→")
    ("\\(=>\\)"              . "⇒")
    ("\\(==\\)"              . "≡")
    ("\\(/=\\)"              . "≠")
    ("\\(=<\\)"              . "≤")
    ("\\(>=\\)"              . "≥")
    ("\\(::\\)"              . "∷")
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

(defun lambda-lifting-fontify (beg end)
  (lambda-lifting-unfontify beg end)
  (dolist (subst lambda-lifting-substitutions)
    (save-excursion
      (goto-char beg)
      (let ((regex (car subst))
            (replacement (cdr subst)))
        (while (re-search-forward regex end t)
          (let* ((b (match-beginning 1))
                 (e (match-end 1))
                 (oldwidth (car (window-text-pixel-size nil b e)))
                 (overlay (make-overlay b e)))
            (overlay-put overlay 'type 'lambda-lifting)
            (overlay-put overlay 'display replacement)
            (let* ((newwidth (car (window-text-pixel-size nil b e)))
                   (remaining (- oldwidth newwidth))
                   (before (/ remaining 2))
                   (after (- remaining before)))
              (overlay-put overlay 'before-string (propertize " " 'display `(space . (:width (,before)))))
              (overlay-put overlay 'after-string (propertize " " 'display `(space . (:width (,after))))))))))))

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