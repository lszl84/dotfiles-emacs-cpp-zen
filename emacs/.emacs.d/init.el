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
  (setq-default mode-line-format nil)

  ;; Touchpad scrolling
  (pixel-scroll-mode 1)
  (pixel-scroll-precision-mode 1))

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

(defun my/setup-org ()
  (use-package org
    :config
    (set-face-attribute 'org-ellipsis nil :underline nil)
    (setq org-ellipsis  " ▼"
	  org-hide-emphasis-markers t
	  org-clock-mode-line-total 'today
          org-duration-format (quote h:mm))
    (add-hook 'org-mode-hook (lambda ()
			       (org-indent-mode)
			       (variable-pitch-mode -1)
			       (auto-fill-mode 1)))))

(defun my/setup-ai ()
  (unless (package-installed-p 'gptel)
    (package-refresh-contents)
    (package-install 'gptel))

  ;; possible workaround for UTF-8 crash on Debian
  (setq gptel-use-curl t)

  ;; was: 'deepseek-reasoner
  (setq gptel-model 'deepseek-chat
	gptel-backend (gptel-make-deepseek "DeepSeek"
					   :stream t
					   :key (with-temp-buffer
						  (insert-file-contents "~/.deepseek-secret")
						  (string-trim (buffer-string))))
	gptel-include-reasoning nil)
  
  (defun gpt-go-to-end (start end)
    (with-current-buffer "*DeepSeek*"
      (goto-char (point-max))
      ))

  (add-hook 'gptel-post-response-functions 'gpt-go-to-end))


(defun my/setup-cmake ()
  (global-set-key (kbd "C-c C-r")
                  (lambda ()
                    (interactive)
                    (compile "cmake --build build -j && ./build/main")
                    (with-current-buffer "*compilation*"
                      (add-hook 'compilation-finish-functions
                                (lambda (buf why)
                                  (when (string-match-p "finished" why)
                                    (let ((win (get-buffer-window buf)))
                                      (when win (delete-window win)))))
                                nil t))))
  (global-set-key (kbd "C-c C-m")
                  (lambda ()
                    (interactive)
                    (compile "cmake -Bbuild -GNinja -DCMAKE_EXPORT_COMPILE_COMMANDS=1 -DCMAKE_BUILD_TYPE=Debug")
                    (with-current-buffer "*compilation*"
                      (add-hook 'compilation-finish-functions
                                (lambda (buf why)
                                  (when (string-match-p "finished" why)
                                    (let ((win (get-buffer-window buf)))
                                      (when win (delete-window win)))))
                                nil t))))



  )

(defun my/setup-shell ()
  (global-set-key (kbd "<s-return>")
		(lambda () (interactive)
		  (ansi-term "/bin/bash")))

  (add-hook 'term-exec-hook
          (lambda ()
            (set-process-sentinel (get-buffer-process (current-buffer))
                                  (lambda (proc change)
                                    (when (memq (process-status proc) '(exit signal))
                                      (kill-buffer (process-buffer proc)))))))

  (setq shell-file-name "/bin/bash"))


(defun my/setup-cpp ()
  (add-hook 'c-mode-common-hook
	    (lambda ()
	      (c-set-style "stroustrup")  
              (setq c-basic-offset 4)
	      (setq indent-tabs-mode nil)
	      ;; we don't disable it, instead use ~/.clang-format
	      ;; with a similar setup as emacs' "stroustrup"
	      ;; clangd formatting is more comprehensive, e.g.
	      ;; automatically putting spaces around operands etc.
	      ;; (setq eglot-ignored-server-capabilities '(:documentOnTypeFormattingProvider))
	      ;; rebinding the default emacs key to do full reformat
	      (local-set-key (kbd "C-M-\\") 'eglot-format-buffer)
	      (eglot-ensure)
	      (local-set-key (kbd "C-c o") 'ff-find-other-file)
	      ))

  (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode)))

;; Main initialization
(defun my/init ()
  "Main initialization function."
  ;; Setup current frame
  (my/setup-theme)
  (my/setup-org)
  (my/setup-ai)
  (my/setup-cmake)
  (my/setup-shell)
  (my/setup-cpp)

  (defun ffplay-media-url ()
  "Open media URL at point with ffplay"
  (interactive)
  (let ((url (get-text-property (point) 'help-echo)))
    (when url
      (async-shell-command 
       (format "ffplay -autoexit '%s'" url)))))
  
  (setq-default backup-directory-alist
		`(("." . "~/.emacs.d/backups")))

  ;; Setup for new frames (important for server mode)
  (add-hook 'after-make-frame-functions #'my/setup-frame)
  
  ;; Server-specific setup
  (when (and (fboundp 'server-running-p) (server-running-p))
    (add-hook 'server-after-make-frame-hook #'my/setup-ui)))

;; Run initialization
(my/init)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
