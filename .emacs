; packages want to install
(setq package-list '(exec-path-from-shell fsharp-mode dash auto-complete grep-a-lot haskell-mode popup pos-tip rust-mode s sml-mode tuareg caml))

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
	("melpa" . "http://melpa.milkbox.net/packages/")
	("marmalade" . "http://marmalade-repo.org/packages/")))

(package-initialize)

; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-cotents))
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

(defun kid-cool-box (title begin end)
  "Wrap the region with a cool box.
The result is like this:
,----------[ Title ]
| This is the marked region
| that will be boxed
`----------
"
  (interactive "sTitle: \nr")
  (setq end (copy-marker end t))
  (save-excursion
    (goto-char begin)
    (unless (looking-back "^")
      (insert "\n"))
    (insert ",----------[ ")
    (insert title)
    (insert " ]\n")
    (while (< (point) end)
      (insert "| ")
      (next-line)
      (beginning-of-line))
    (goto-char end)
    (unless (looking-back "^")
      (insert "\n"))
    (insert "`----------\n")))

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
(global-set-key (kbd "C-z") 'scroll-n-lines-ahead)

(defun point-to-top ()
  "Put point on top line of window."
  (interactive)
  (move-to-window-line 0))
(defun point-to-bottom ()
  "Put point at beginning of last visible line."
  (interactive)
  (move-to-window-line -1))

(global-set-key (kbd "C-,") 'point-to-top)
(global-set-key (kbd "C-.") 'point-to-bottom)
