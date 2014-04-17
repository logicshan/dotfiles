; packages want to install
(setq package-list '(ascii exec-path-from-shell fsharp-mode dash auto-complete grep-a-lot haskell-mode popup pos-tip rust-mode s sml-mode tuareg caml))

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
	("melpa" . "http://melpa.milkbox.net/packages/")
	("marmalade" . "http://marmalade-repo.org/packages/")))

(package-initialize)

; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))
; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(require 'grep-a-lot)
(grep-a-lot-setup-keys)

(when (memq window-system '(x ns))
  (exec-path-from-shell-initialize))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(haskell-mode-hook (quote (turn-on-haskell-decl-scan turn-on-haskell-indentation)))
 '(haskell-program-name "ghci")
 '(ido-enable-flex-matching t)
 '(ido-mode (quote both) nil (ido))
 '(inhibit-startup-echo-area-message "shanning")
 '(inhibit-startup-screen t)
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(initial-scratch-message nil)
 '(menu-bar-mode nil)
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style (quote post-forward) nil (uniquify)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(add-to-list 'load-path "~/.emacs.d/site-lisp/")

(add-to-list 'auto-mode-alist '("\\.m\\'" . miranda-mode))
(autoload 'miranda-mode "miranda-mode"
  "Major mode for editing Miranda scripts" t nil)

(add-to-list 'auto-mode-alist '("\\.ott\\'" . ott-mode))
(autoload 'ott-mode "ottmode"
  "Major mode for editing Ott files." t nil)

(let ((file (expand-file-name "~/quicklisp/slime-helper.el")))
  (if (file-exists-p file)
      (load file)))
(setq inferior-lisp-program "sbcl")

;; "Miscellaneous Symbols And Pictographs" U+1F300 - U+1F5FF
;; "Emoticons"  U+1F600 - U+1F64F
(set-fontset-font "fontset-default"
		  (cons (decode-char 'ucs #x1f300)
			(decode-char 'ucs #x1f64f))
		  "Segoe UI Symbol")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun other-window-backward (&optional n)
  "Select Nth previous window."
  (interactive "P")
  (other-window (- (prefix-numeric-value n))))

(global-set-key (kbd "C-x C-n") 'other-window)
(global-set-key (kbd "C-x C-p") 'other-window-backward)

(defalias 'scroll-ahead 'scroll-up)
(defalias 'scroll-behind 'scroll-down)

(defun scroll-n-lines-ahead (&optional n)
  "Scroll ahead N lines (1 by default)."
  (interactive "P")
  (scroll-ahead (prefix-numeric-value n)))
(defun scroll-n-lines-behind (&optional n)
  "Scroll behind N lines (1 by default)."
  (interactive "P")
  (scroll-behind (prefix-numeric-value n)))

(global-set-key (kbd "C-q") 'scroll-n-lines-behind)
(define-key minibuffer-local-map (kbd "C-q") 'quoted-insert)
(global-set-key (kbd "C-z") 'scroll-n-lines-ahead)

(defun read-only-if-symlink ()
  (if (file-symlink-p buffer-file-name)
      (progn
	(setq buffer-read-only t)
	(message "File is a symlink"))))
(add-hook 'find-file-hooks 'read-only-if-symlink)

(defun visit-target-instead ()
  "Replace this buffer with a buffer visiting the link target."
  (interactive)
  (if buffer-file-name
      (let ((target (file-symlink-p buffer-file-name)))
	(if target
	    (find-alternate-file target)
	  (error "Not visiting a symlink")))
    (error "Not visiting a file")))

(defun clobber-symlink ()
  "Replace symlink with a copy of the file."
  (interactive)
  (if buffer-file-name
      (let ((target (file-symlink-p buffer-file-name)))
	(if target
	    (if (yes-or-no-p (format "Replace %s with %s"
				     buffer-file-name
				     target))
		(progn
		  (delete-file buffer-file-name)
		  (write-file buffer-file-name)))
	  (error "Not visiting a symlink")))
    (error "Not visiting a file")))

(defadvice switch-to-buffer (before existing-buffer
				    activate compile)
  "When interactive, switch to existing buffers only,
unless given a prefix argument."
  (interactive
   (list (read-buffer "Switch to buffer: "
		      (other-buffer)
		      (null current-prefix-arg)))))

(put 'scroll-up 'unscrollable t)
(put 'scroll-down 'unscrollable t)
(put 'scroll-left 'unscrollable t)
(put 'scroll-right 'unscrollable t)

(defvar unscroll-point (make-marker)
  "Cursor position for next call to 'unscroll'.")
(defvar unscroll-window-start (make-marker)
  "Window start for next call to 'unscroll'.")
(defvar unscroll-hscroll nil
  "Hscroll for next call to 'unscroll'.")

(defun unscroll-maybe-remember ()
  (if (not (get last-command 'unscrollable))
      (progn
	(set-marker unscroll-point (point))
	(set-marker unscroll-window-start (window-start))
	(setq unscroll-hscroll (window-hscroll)))))

(defun unscroll ()
  "Revert to 'unscroll-point' and 'unscroll-window-start'."
  (interactive)
  (goto-char unscroll-point)
  (set-window-start nil unscroll-window-start)
  (set-window-hscroll nil unscroll-hscroll))

(defadvice scroll-up (before remember-for-unscroll
			     activate compile)
  "Remember where we started from, for 'unscroll'."
  (unscroll-maybe-remember))
(defadvice scroll-down (before remember-for-unscroll
			       activate compile)
  "Remember where we started from, for 'unscroll'."
  (unscroll-maybe-remember))
(defadvice scroll-left (before remember-for-unscroll
			       activate compile)
  "Remember where we started from, for 'unscroll'."
  (unscroll-maybe-remember))
(defadvice scroll-right (before remember-for-unscroll
				activate compile)
  "Remember where we started from, for 'unscroll'."
  (unscroll-maybe-remember))

(require 'timestamp)

(autoload 'refill-mode "refill"
  "Refill minor mode."
  t)
(autoload 'showmsg-mode "showmsg"
  "Showmsg minor mode."
  t)
(autoload 'quip-mode "quip"
  "Quip major mode."
  t)
(autoload 'crossword "crossword"
  "Create a new buffer with an empty crossword grid."
  t)
(autoload 'crossword-mode "crossword"
  "Major mode for editing crossword puzzles."
  t)
