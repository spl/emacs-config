;; Disable line wrapping
(set-default 'truncate-lines t)

;; Set up package
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

(defvar prelude-packages
  '(company dash dash-functional flycheck f fill-column-indicator s lua-mode mmm-mode ; for lean-mode
            evil)
  "Packages installed at launch.")

(defun prelude-packages-installed-p ()
  (loop for p in prelude-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

;; Check for new package versions
(unless (prelude-packages-installed-p)
  (message "%s" "Emacs Prelude is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; install the missing packages
  (dolist (p prelude-packages)
    (when (not (package-installed-p p))
      (package-install p))))

(provide 'prelude-packages)

;; Set up lean-mode
(setq lean-rootdir "/usr/local")
(setq-local lean-emacs-path
            (concat (file-name-as-directory lean-rootdir)
                    "share/emacs/site-lisp/lean"))
(add-to-list 'load-path (expand-file-name lean-emacs-path))
; Only load lean-mode if lean-emacs-path exists
(when (file-directory-p lean-emacs-path)
      (require 'lean-mode))

;; Set up evil
(require 'evil)
(evil-mode 1)

;; Esc quits everything
;; From https://github.com/mbriggs/.emacs.d/blob/master/config/keybinds.el
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(global-set-key [escape] 'keyboard-quit)
