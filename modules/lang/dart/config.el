;;; lang/dart/config.el -*- lexical-binding: t; -*-

(defun flutter-hot-reload()
	(interactive)
	"Send a signal to daemon to hot reload."
	(shell-command "kill -s SIGUSR1 (cat /tmp/flutter.pid)"))

(defun flutter-hot-reload-enable ()
	(interactive)
	"Enable flutter hot reload on save."
	(add-hook 'after-save-hook 'flutter-hot-reload t t))

(defun flutter-hot-reload-disable ()
	(interactive)
	"Disable flutter hot reload on save."
	(remove-hook 'after-save-hook 'flutter-hot-reload t))


(after! projectile
  (add-to-list 'projectile-project-root-files "pubspec.yaml"))

(use-package dart-mode
  :mode ("\\.dart$" . dart-mode)
  :init (add-hook 'dart-mode-hook 'flutter-hot-reload-enable))

(use-package lsp-mode :ensure t)
(use-package lsp-dart
  :hook (dart-mode . lsp)
  :config
  (require 'dap-ui)
  (require 'dap-mouse)

  ;;(require 'dap-chrome)
  ;; (dap-register-debug-template "Flutter :: Chromium debug"
  ;;                              (list :type "chrome"
  ;;                                    :name "Chrome attach"
  ;;                                    :request "launch"
  ;;                                    :mode "url"
  ;;                                    :url "http://localhost:8080"
  ;;                                    :runtimeExecutable "/snap/bin/chromium"))
  (setq lsp-dart-sdk-dir "~/snap/flutter/common/flutter/bin/cache/dart-sdk/"
        lsp-dart-flutter-sdk-dir "~/snap/flutter/common/flutter"))

(use-package lsp-ui)
(use-package hover)
