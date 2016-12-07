;;; evil-opener.el --- opening urls as buffers in evil

;; Author: Tim Reddehase <tr@rightsrestricted.com>
;; Version: 0.2.2
;; Package-Requires: ((evil "1.2.12") (opener "0.2.2"))
;; Keywords: url, http, files
;; URL: https://github.com/0robustus1/opener.el

;; Copyright (C) 2016 Tim Reddehase
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; opener.el is a small package that provides the user with the ability to open
;; files from http-like sources directly inside an Emacs buffer.  This means that
;; if the URL in question gives string hints to be a plaintext file, that isn't
;; automatically being rendered into a pleasant representation, like for example
;; html files are, it will be opened inside an Emacs buffer.
;;
;; The current main example for this is opening URLs that yield XML or JSON
;; responses, which are potentially even gzipped (think sitemaps for example).
;;
;; This package allows one to hook opener into evil.
;; It defines an :opener ex-state command as well as a remapping of gf (normal
;; state) to opener.

;; Full documentation is available as an Info manual.

;;; Code:
(require 'evil)
(require 'opener)

;;;###autoload
(define-minor-mode evil-opener-mode
  "Buffer-local minor mode to activate evil-opener remappings."
  :keymap (make-sparse-keymap)
  (evil-normalize-keymaps))

;;;###autoload
(define-globalized-minor-mode global-evil-opener-mode
  evil-opener-mode (lambda () (evil-opener-mode 1))
  "Global minor mode to activate evil-opener remappings.")

(evil-define-command opener-evil-open (url-or-file &optional bang)
  "Open URL-OR-FILE. If the url doesn't have the scheme http:// or https:// it
  falls back to be equivalent to :edit"
  :repeat nil
  (interactive "<a><!>")
  (opener-try-open url-or-file bang #'evil-edit))

(evil-define-key 'normal evil-opener-mode-map "gf" 'opener-open-at-point)
(evil-ex-define-cmd "o[pener]" 'opener-evil-open)

(provide 'evil-opener)
;;; evil-opener.el ends here
