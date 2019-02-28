(toggle-frame-maximized)

(add-to-list 'load-path "/home/shanning/test/cubicaltt")
(autoload 'cubicaltt-mode "cubicaltt" "cubical editing mode" t)
(setq auto-mode-alist (append auto-mode-alist '(("\\.ctt$" . cubicaltt-mode))))


(load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda-mode locate")))

(eval-after-load "quail/latin-ltx"
  '(mapc (lambda (pair)
	   (quail-defrule (car pair) (cadr pair) "TeX"))
	 '(("\\bl" "𝕃") ("\\bs" "𝕊")
	   ("\\bt" "𝕋") ("\\bv" "𝕍"))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-command "latex")
 '(coq-compiler "coqtop")
 '(coq-prog-name "coqtop")
 '(gap-executable "/home/shanning/Development/gap4r8/bin/gap.sh")
 '(gap-start-options (quote ("-f" "-b" "-m" "2m" "-E")))
 '(inhibit-startup-screen t)
 '(package-selected-packages
   (quote
    (z3-mode markdown-preview-mode markdown-preview-eww inf-ruby racket-mode redprl fill-column-indicator window-numbering unicode-fonts smex ido-yes-or-no haskell-mode gnuplot-mode gap-mode flycheck f dash-functional company-coq auto-complete-sage auctex async)))
 '(redprl-command "/home/shanning/Documents/sml-redprl/bin/redprl")
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'agda-input)
(add-hook 'haskell-mode-hook
	  (lambda () (set-input-method "Agda")))

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


;; Open .v files with Proof General's Coq mode
;(load "~/.emacs.d/lisp/PG/generic/proof-site")


(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

;; Install required/optional packages for lean-mode
(defvar lean-mode-required-packages
  '(company dash dash-functional flycheck f
            fill-column-indicator s))
(let ((need-to-refresh t))
  (dolist (p lean-mode-required-packages)
    (when (not (package-installed-p p))
      (when need-to-refresh
        (package-refresh-contents)
        (setq need-to-refresh nil))
      (package-install p))))

;; math input method setup
(require 'math-symbol-lists)
(quail-define-package "math" "UTF-8" "Ω" t)
(quail-define-rules ; add whatever extra rules you want to define here...
 ("\\from"    #X2190)
 ("\\to"      #X2192)
 ("\\lhd"     #X22B2)
 ("\\rhd"     #X22B3)
 ("\\unlhd"   #X22B4)
 ("\\unrhd"   #X22B5))
(mapc (lambda (x)
        (if (cddr x)
            (quail-defrule (cadr x) (car (cddr x)))))
      (append math-symbol-list-basic math-symbol-list-extended))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(eval-after-load "latex"
  '(mapc (lambda (key-cmd) (define-key LaTeX-mode-map (car key-cmd) (cdr key-cmd)))
         `((,(kbd "C-c s c") . sage-shell-sagetex:compile-current-file)
           (,(kbd "C-c s C") . sage-shell-sagetex:compile-file)
           (,(kbd "C-c s r") . sage-shell-sagetex:run-latex-and-load-current-file)
           (,(kbd "C-c s R") . sage-shell-sagetex:run-latex-and-load-file)
           (,(kbd "C-c s l") . sage-shell-sagetex:load-current-file)
           (,(kbd "C-c s L") . sage-shell-sagetex:load-file)
           (,(kbd "C-c C-z") . sage-shell-edit:pop-to-process-buffer))))



(ido-mode 1)
(ido-everywhere 1)

(require 'ido-completing-read+)
(ido-ubiquitous-mode 1)

(require 'ido-yes-or-no)
(ido-yes-or-no-mode 1)

(require 'smex)
(smex-initialize)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is the old M-x
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(require 'window-numbering)
(window-numbering-mode 1)

(add-hook 'gap-mode-hook
	  (lambda ()
	    (define-key gap-mode-map (kbd "C-c C-n") 'gap-eval-last-statement)
	    (define-key gap-mode-map (kbd "M-n") 'gap-completion)))
