(defun short-line-p (pos)
  "Does line containing POS stay within `fill-column'?"
  (save-excursion
    (goto-char pos)
    (end-of-line)
    (<= (current-column) fill-column)))

(defun same-line-p (start end)
  "Are START and END on the same line?"
  (save-excursion
    (goto-char start)
    (end-of-line)
    (<= end (point))))

(defun showmsg (start end len)
  "After a text change, print some messages."
  (message (format "start: %d; end: %d; len: %d" start end len))
  (message (format "preceding-char: %c" (preceding-char)))
  (message (format "(zerop len)             -> %s" (zerop len)))
  (message (format "(same-line-p start end) -> %s" (same-line-p start end)))
  (message (format "(short-line-p end)      -> %s" (short-line-p end)))
  (message (format "(and (zerop len) (same-line-p start end) (short-line-p end)) -> %s"
		  (and (zerop len) (same-line-p start end) (short-line-p end))))
  (message (format "(and (eq (char-syntax (preceding-char)) ?\\ ) (looking-at \"\\s *$\"))) -> %s\n"
		   (and (eq (char-syntax (char-before end)) ?\ ) (looking-at "\\s *$")))))

(defvar showmsg-mode nil
  "Mode variable for showmsg minor mode.")
(make-variable-buffer-local 'showmsg-mode)

(if (not (assq 'showmsg-mode minor-mode-alist))
    (setq minor-mode-alist
          (cons '(showmsg-mode " Showmsg")
                minor-mode-alist)))

(defun showmsg-mode (&optional arg)
  "Showmsg minor mode."
  (interactive "P")
  (setq showmsg-mode
        (if (null arg)
            (not showmsg-mode)
          (> (prefix-numeric-value arg) 0)))
  (if showmsg-mode
      (add-hook 'after-change-functions 'showmsg nil t)
    (remove-hook 'after-change-functions 'showmsg t)))

(provide 'showmsg)
