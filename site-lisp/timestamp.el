(defgroup timestamp nil
  "Timestamp group."
  :group 'local)

(defcustom insert-time-format "%X"
  "Format for \\[insert-time] (c.f. `format-time-string')."
  :group 'timestamp)
(defcustom insert-date-format "%x"
  "Format for \\[insert-date] (c.f. `format-time-string')."
  :group 'timestamp)

(defun insert-time ()
  "Insert the current time according to insert-time-format."
  (interactive "*")
  (insert (format-time-string insert-time-format
			      (current-time))))
(defun insert-date ()
  "Insert the current date according to insert-date-format."
  (interactive "*")
  (insert (format-time-string insert-date-format
			      (current-time))))

(defcustom writestamp-format "%Y-%m-%d %H:%M:%S"
  "Format for writestamps (c.f. `format-time-string')."
  :group 'timestamp)
(defcustom writestamp-prefix "WRITESTAMP(("
  "Unique string identifying start of writestamp."
  :group 'timestamp)
(defcustom writestamp-suffix "))"
  "String that terminates a writestamp."
  :group 'timestamp)

(defun insert-writestamp ()
  "Insert a writestamp"
  (interactive "*")
  (end-of-line)
  (newline)
  (insert writestamp-prefix)
  (insert (format-time-string writestamp-format (current-time)))
  (insert writestamp-suffix)
  (newline))

(defun update-writestamps ()
  "Find writestamps and replace them with the current time."
  (save-excursion
    (save-restriction
      (save-match-data
	(widen)
	(goto-char (point-min))
	(let ((regexp (concat "^"
			      (regexp-quote writestamp-prefix)
			      "\\(.*\\)"
			      (regexp-quote writestamp-suffix)
			      "$")))
	  (while (re-search-forward regexp nil t)
	    (replace-match (format-time-string writestamp-format (current-time))
			   t t nil 1))))))
  nil)

(defvar last-change-time nil
  "Time of last buffer modification.")
(make-variable-buffer-local 'last-change-time)

(defun remember-change-time (&rest unused)
  "Store the current time in `last-change-time'."
  (setq last-change-time (current-time)))

(defcustom modifystamp-format "%Y-%m-%d %H:%M:%S"
  "Format for modifystamps (c.f. `format-time-string')."
  :group 'timestamp)

(defcustom modifystamp-prefix "MODIFYSTAMP(("
  "String identifying start of modifystamp."
  :group 'timestamp)

(defcustom modifystamp-suffix "))"
  "String that terminates a modifystamp."
  :group 'timestamp)

(defun insert-modifystamp ()
  "Insert a modifystamp"
  (interactive "*")
  (end-of-line)
  (newline)
  (insert modifystamp-prefix)
  (insert (format-time-string modifystamp-format (current-time)))
  (insert modifystamp-suffix)
  (newline))

(defun maybe-update-modifystamps ()
  "Call `update-modifystamps' if the buffer has been modified."
  (if last-change-time ; instead of testing (buffer-modified-p)
      (update-modifystamps last-change-time)))

(defun update-modifystamps (time)
  "Find modifystamps and replace them with the given time."
  (save-excursion
    (save-restriction
      (save-match-data
        (widen)
        (goto-char (point-min))
        (let ((regexp (concat "^"
                              (regexp-quote modifystamp-prefix)
                              "\\(.*\\)"
                              (regexp-quote modifystamp-suffix)
                              "$")))
          (while (re-search-forward regexp nil t)
            (replace-match (format-time-string modifystamp-format time)
                           t t nil 1))))))
  (setq last-change-time nil)
  nil)

(provide 'timestamp)
