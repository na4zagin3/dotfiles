(setq exec-path (append '("/Users/mrty/.cabal/bin" "/Users/mrty/.opam/system/bin" "/usr/local/share/npm/bin" "/usr/local/bin") exec-path))
(setq custom-file "~/.emacscustom")
(if (file-exists-p (expand-file-name custom-file))
    (load (expand-file-name custom-file) t nil nil))

(package-initialize)
;(package-install 'evil)
;(package-install 'evil)

;(load-file (expand-file-name "~/.emacs.d/init.el"))

;;  EVIL mode
;(add-to-list 'load-path (expand-file-name "~/.emacs.d/evil"))
(require 'undo-tree)
(require 'evil)
(evil-mode 1)

;;(require 'viper)
;;(viper-mode)
;;  (defun my-viper-beginning-of-buffer ()
;;    (interactive)
;;    (beginning-of-buffer))
;;  (define-key viper-vi-global-user-map [?g?g] 'my-viper-beginning-of-buffer)

;;  (defun my-viper-star ()
;;    (interactive)
;;    (let ((wd (concat "\\<" (thing-at-point 'symbol) "\\>")))
;;      (setq viper-s-string wd)
;;      (setq viper-s-forward t)
;;      (viper-search wd t 1)))
;;  (define-key viper-vi-global-user-map [?*] 'my-viper-star)

;;   (defun my-viper-jump-tag ()
;;     (interactive)
;;     (setq wd (thing-at-point 'symbol))
;;     (find-tag wd))
;;   (define-key viper-vi-global-user-map [?\C-\]] 'my-viper-jump-tag)
;; 
;;   (defun my-viper-jump-tag-next ()
;;     (interactive)
;;     (setq wd (thing-at-point 'symbol))
;;     (find-tag wd 0))
;;   (define-key viper-vi-global-user-map [?\C-:] 'my-viper-jump-tag-next)
;; 
;;   (defun my-viper-pop-tag ()
;;     (interactive)
;;     (pop-tag-mark))
;;   (define-key viper-vi-global-user-map [?\C-t] 'my-viper-pop-tag)
;; 
;;   (defun my-viper-pop-mark ()
;;     (interactive)
;;     (set-mark-command -1))
;;   (define-key viper-vi-global-user-map [?\C-o] 'my-viper-pop-mark)
;; 
;;   (define-key viper-vi-global-user-map [?u] 'undo)
;;   (define-key viper-insert-global-user-map [backspace] 'backward-delete-char-untabify)
;;   (define-key viper-insert-global-user-map [delete] 'delete-char)
;;   (define-key viper-emacs-global-user-map "\C-w\C-w" 'other-window)
;;   (define-key viper-vi-global-user-map "\C-w\C-w" 'other-window)
;;   (define-key viper-emacs-global-user-map "\C-w\C-o" 'delete-other-windows)
;;   (define-key viper-vi-global-user-map "\C-w\C-o" 'delete-other-windows)
;; 
;;   ;; 
;;   (define-key viper-dired-modifier-map "j" 'dired-next-line)
;;   (define-key viper-dired-modifier-map "k" 'dired-previous-line)
;;   (define-key viper-dired-modifier-map "/" 'dired-goto-file)
;;   (define-key viper-dired-modifier-map "l" '(lambda () (interactive) (dired-next-line 10)))
;;   (define-key viper-dired-modifier-map "h" '(lambda () (interactive) (dired-previous-line 10)))
;; 
;;   ;;%%%
;;   (viper-set-parsing-style-toggling-macro t)

;(require 'apl)


;; (setq viper-mode t)
;; (setq viper-inhibit-startup-message 't)
;; (require 'viper)

(require 'w3m)

;(require 'color-theme)
;(color-theme-initialize)
;(color-theme-arjen)
;(color-theme-billw)
;calm-forest
;chacoal-black
;clarity
;commidia
;dark-laptop
;europhia


;(color-theme-bharadwaj-slate)
(setq w3m-use-cookie t)


;(require 'scheme-complete)
;(require 'quack)
;(autoload 'scheme-smart-complete "scheme-complete" nil t)
;(add-hook 'scheme-mode-hook
;   (lambda ()
;     (make-local-variable 'eldoc-documentation-function)
;     (setq eldoc-documentation-function 'scheme-get-current-symbol-info)
;     (eldoc-mode)))
; (eval-after-load 'scheme
;   '(define-key scheme-mode-map "\t" 'scheme-complete-or-indent))
;(setq list-indent-function 'scheme-smart-indent-function)

(show-paren-mode t)

;;Hasell Mode
;(load "/usr/share/emacs/site-lisp/haskell-mode/haskell-site-file.el")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)

(setq auto-mode-alist (cons '("\\.ml\\w?" . tuareg-mode) auto-mode-alist))
(autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)

;;; qee undo-tre3
;;;
;;; * (add-hook 'haskell-mode-hook (lambda () (ghc-init)))
;;;  * or if you wish to use flymake, instead add this one line to ~/.emacs:
;;;   * (add-hook 'haskell-mode-hook (lambda () (ghc-init) (flymake-mode)))
;;;   >

;(require 'scala-mode-auto)

;; scala mode applied to X10 files
;(add-to-list 'load-path (expand-file-name "~/.emacs.d/x10-mode"))
;(require 'x10-mode-auto)

(require 'org-install)
(setq org-directory "~/org")                            ; orgディレクトリ
(setq org-todo-keywords '((type "TODO(t)" "STARTED(s)" "WAITING(w)" "APPT(a)" "|" "DONE(d)" "CANCELLED(c)" "DEFERRED(f)")))
(setq org-mobile-directory "~/Dropbox/mobileorg")       ; MobileOrg用ディレクトリ

(setq org-tag-alist '(("ANY" . ?a) ("HOME" . ?h) ("WORK" . ?w) ("OUTGO" . ?o)))
;; (setq org-agenda-files (list org-directory))
(defun vimorg-tag-adjust ()
  (interactive)
   (while (re-search-forward "^*.*?\n[ \t]+:[^ \t]+:" nil t)
         (if (not (string-match "\\(PROPERTIES\\|LOGBOOK\\)" (thing-at-point 'line)))
              (join-line))))

(defun vimorg-set-unmodified ()
    (interactive)
    (set-buffer-modified-p nil))

(add-hook 'org-mode-hook
(lambda () (interactive)(replace-regexp "\\(\\s-*\\):\\(DEADLINE\\|CLOSED\\|SCHEDULED\\|CLOCK\\|<[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} \\)" "\\1\\2")
                (beginning-of-buffer)(vimorg-tag-adjust)
        (beginning-of-buffer) ))

;; proof general
;(load-file "/usr/local/share/emacs/site-lisp/ProofGeneral/generic/proof-site.el")
