;;; opener.el --- opening urls as buffers

;; Author: Tim Reddehase <tr@rightsrestricted.com>
;; Version: 0.1
;; Package-Requires ((request "20160822.1659"))
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

;; Full documentation is available as an Info manual.

;;; Code:
(require 'request)
(require 'evil)
(eval-when-compile (require 'cl)) ;; lexical-let

(defun opener-filename-for (url)
  "Convert a URL into a valid file-path.
Seeing as one might open multiple URL-file buffers, it is useful to distinguish
them by more than their base-name"
  (replace-regexp-in-string "http[s]?://" "" url))

(defun opener-perform-major-mode-hooks ()
  "Perform necessary hooks for the determined file-mode.
Pretty printing is the usual example here, as most representations one the web
occur in minified format, which is not particularly pleasent for humans."
  (if (derived-mode-p 'nxml-mode)
      (nxml-pretty-format)))

(defun opener-http-response-in-buffer (buffer-name data)
  "Actually create a buffer named BUFFER-NAME and fill it with DATA.
During this process it also attempts decompression, determines
correct major-modes and performs hooks.
It also makes that buffer current."
  (let ((buffer (get-buffer-create buffer-name)))
    (save-excursion
      (with-current-buffer buffer
        (erase-buffer)
        (if enable-multibyte-characters (toggle-enable-multibyte-characters))
        (insert data)
        (zlib-decompress-region (point-min) (point-max))
        (normal-mode)
        (opener-perform-major-mode-hooks)))
    (switch-to-buffer buffer)))

(defun opener-open-url-in-buffer (url)
  "Open URL in an aptly named buffer."
  (request
   url
   :parser 'buffer-string
   :complete (lexical-let ((buffer-name (opener-filename-for url)))
               (function*
                (lambda (&key data &allow-other-keys)
                  (opener-http-response-in-buffer buffer-name data))))))

(evil-define-command opener-open (url-or-file &optional bang)
  "Open URL-OR-FILE. If the url doesn't have the scheme http:// or https:// it
  falls back to be equivalent to :edit"
  :repeat nil
  (interactive "<f><!>")
  (if (or
       (string-prefix-p "http://" url-or-file)
       (string-prefix-p "https://" url-or-file))
      (opener-open-url-in-buffer url-or-file)
    (evil-edit url-or-file bang)))

(evil-ex-define-cmd "o[pener]" 'opener-open)
;;; opener.el ends here
