(if (> emacs-minor-version 3)
    (toggle-frame-fullscreen) ; This is bound to f11 in Emacs 24.4
  (setq initial-frame-alist '((fullscreen . maximized)))
  (setq user-emacs-directory "~/.emacs24.3.d/"))

; packages want to install
(setq package-list '(ascii exec-path-from-shell haskell-mode rust-mode))

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
