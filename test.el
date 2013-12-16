(defvar buster-test-regexp
  "^\s+\"\.+\s\.+\":\s?fun"
  "Regular expression that finds the begining of a test function")

(defun buster-test-name-pos ()
  "Return the position where the test name starts on the current line"
  (save-excursion
    (beginning-of-line)
    (search-forward "\"" (point-at-eol))
    (1- (point))))

(defun buster-beginning-of-test-curr-linep ()
  "Return t if a test starts on the current line"
  (save-excursion
    (beginning-of-line)
    (search-forward-regexp buster-test-regexp (point-at-eol) t)))

(defun buster-goto-beginning-of-test ()
  "Move point to the beginning of the current test function.
Does nothing if point is not currently on a line where a test is declared."
  (interactive)
  (if (buster-beginning-of-test-curr-linep)
      (goto-char (buster-test-name-pos))))

(global-set-key (kbd "C-c t") 'buster-goto-beginning-of-test)

(defun buster-beginning-of-test-pos ()
  "Return the start position of the current test"
  (let ((curr (point))
	(start-pos 0)
	(end-pos 0))
    (save-excursion
      (search-backward-regexp buster-test-regexp)
      (setq start-pos (point))
      (setq end-pos (buster-goto-eoblock))
      (if (and (< start-pos curr)
	       (< curr end-pos))
	  start-pos curr))))

(defun buster-find-next-pos (char)
  "Return the position at the next occurrence of `char`"
  (save-excursion
    (if (not (search-forward char nil t)) (end-of-buffer))
    (point)))

(defun buster-goto-eoblock (&optional open-paren-pairs-count)
  "Move point to the end of the next block"
  (if (not open-paren-pairs-count)
      (progn
	(search-forward "{")
	(setq open-paren-pairs-count 1)))
  (cond
   ((eq 0 open-paren-pairs-count) (point))
   (t (let ((open (buster-find-next-pos "{"))
	    (close (buster-find-next-pos "}")))
	(cond
	 ((< open close)
	  (goto-char open)
	  (buster-goto-eoblock (1+ open-paren-pairs-count)))
	 ((< close open)
	  (goto-char close)
	  (buster-goto-eoblock (1- open-paren-pairs-count))))))))
