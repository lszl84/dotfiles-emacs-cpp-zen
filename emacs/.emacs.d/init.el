;; Initialize package sources
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; Basic UI configuration
(defun my/setup-ui ()
  "Configure UI elements for current frame."
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (setq-default mode-line-format nil))

;; Theme and appearance setup
(defun my/setup-theme ()
  "Configure theme and colors."
  (load-theme 'luke t)
  (my/setup-ui))

;; Server/client frame handling
(defun my/setup-frame (frame)
  "Configure FRAME for consistent appearance."
  (with-selected-frame frame
    (my/setup-theme)))

;; Main initialization
(defun my/init ()
  "Main initialization function."
  ;; Setup current frame
  (my/setup-theme)
  
  (setq-default backup-directory-alist
		`(("." . "~/.emacs.d/backups")))

  ;; Setup for new frames (important for server mode)
  (add-hook 'after-make-frame-functions #'my/setup-frame)
  
  ;; Server-specific setup
  (when (and (fboundp 'server-running-p) (server-running-p))
    (add-hook 'server-after-make-frame-hook #'my/setup-ui)))

;; Run initialization
(my/init)

