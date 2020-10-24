;;; lang/dart/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +flutter-project-p ()
  "Return t if this is a flutter project."
  (locate-dominating-file buffer-file-name "pubspec.yml"))
