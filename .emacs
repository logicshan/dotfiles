(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(browse-url-browser-function (quote browse-url-firefox))
 '(column-number-mode t)
 '(flymake-gui-warnings-enabled t)
 '(haskell-font-lock-symbols t)
 '(haskell-hoogle-command "hoogle")
 '(haskell-program-name "ghci")
 '(prolog-system (quote swi))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flymake-errline ((t (:inherit nil)))))

(add-hook 'haskell-mode-hook (lambda () (require 'inf-haskell)))

(add-hook 'agda2-mode-hook
	  '(lambda () (abbrev-mode 1)))
(add-hook 'coq-mode-hook
	  '(lambda ()
	     (abbrev-mode 1)
	     (local-set-key (kbd "C-c C-/") 'proof-goto-point)))

(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(autoload 'ghc-init "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init) (flymake-mode)))

; solve exec-path doesn't equal with PATH
(when (memq window-system '(x ns))
  (exec-path-from-shell-initialize))

(require 'hs-lint)
(defun my-haskell-mode-hook ()
  (local-set-key (kbd "C-c l") 'hs-lint))
(add-hook 'haskell-mode-hook 'my-haskell-mode-hook)

(add-to-list 'auto-mode-alist '("\\.pl$" . prolog-mode))

(require 'ibus)
;; Turn on ibus-mode automatically after loading .emacs
(add-hook 'after-init-hook 'ibus-mode-on)
;; Use C-SPC for Set Mark command
(ibus-define-common-key ?\C-\s nil)
;; Use C-/ for Undo command
(ibus-define-common-key ?\C-/ nil)
;; Change cursor color depending on IBus status
(setq ibus-cursor-color '("red" "blue" "limegreen"))
;; Use S-SPC to toggle input status
(ibus-define-common-key ?\S-\s nil)
(global-set-key (kbd "S-SPC") 'ibus-toggle)


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
