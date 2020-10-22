;;; core/autoload/system.el -*- lexical-binding: t; -*-

;;;###autoload
(defun doom-system-os (&optional os)
  "Returns the OS: arch, debian, macos, general linux, cygwin or windows. If OS
is given, returns t if it matches the current system, and nil otherwise."
  (let* ((gnu-linux-p (eq system-type 'gnu/linux))
         (type (cond ((and gnu-linux-p (file-exists-p "/etc/arch-release"))
                      'arch)
                     ((and gnu-linux-p (file-exists-p "/etc/debian_version"))
                      'debian)
                     (gnu-linux-p
                      'linux)
                     ((eq system-type 'darwin)
                      'macos)
                     ((memq system-type '(windows-nt cygwin))
                      'windows)
                     (t (error "Unknown OS: %s" system-type)))))
    (or (and os (eq os type))
        type)))
