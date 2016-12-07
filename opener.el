;;; opener.el --- opening urls as buffers -*- lexical-binding: t; -*-

;; Author: Tim Reddehase <tr@rightsrestricted.com>
;; Version: 0.2.2
;; Package-Requires: ((request "0.2.0") (emacs "24") (cl-lib "0.5"))
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
(require 'ffap)
(require 'cl-lib)

(defgroup opener nil
  "opening urls as buffers"
  :group 'convenience)

(defcustom opener-major-mode-hooks
  '()
  "List of 'major-mode' to list of functions to be executed.
When opening a buffer that matches one of the modes, the functions are applied
with the buffer being the current one.  This allows for e.g. pretty-printing."
  :group 'opener
  :type '(repeat function))

(defcustom opener-url-browser-function
  'browse-url
  "Represents the function to be called in order to invoke a browser.
This usually only happens in the case of directory-style URLs.  Customize this
variable to force your own browser, to use xwidgets (xwidget-webkit-browse-url)
or to provide your own implementation.  The function takes one argument (the
URL)."
  :group 'opener
  :type 'function)

(defun opener-filename-for (url)
  "Convert a URL into a valid file-path.
Seeing as one might open multiple URL-file buffers, it is useful to distinguish
them by more than their base-name"
  (replace-regexp-in-string "http[s]?://" "" (url-unhex-string url)))

(defun opener-file-like-url (url)
  "Report whether the URL seems like it corresponds to a normal file."
  (let ((file-segment (car (last (split-string url "/")))))
    (file-name-extension file-segment)))

(defun opener-perform-major-mode-hooks ()
  "Perform necessary hooks for the determined file-mode.
Pretty printing is the usual example here, as most representations one the web
occur in minified format, which is not particularly pleasent for humans."
  (dolist (mapping opener-major-mode-hooks)
    (when (derived-mode-p (car mapping))
      (dolist (func (cadr mapping))
        (funcall func)))))

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
        (let ((buffer-file-name buffer-name)) (normal-mode))
        (opener-perform-major-mode-hooks)))
    (switch-to-buffer buffer)))

(defun opener-open-url-in-buffer (url)
  "Open URL in an aptly named buffer."
  (request
   url
   :parser 'buffer-string
   :complete (let ((buffer-name (opener-filename-for url)))
               (function*
                (lambda (&key data &allow-other-keys)
                  (opener-http-response-in-buffer buffer-name data))))))

(defun opener-supported-url-scheme-p (url)
  "If URL is http or https, nil otherwise."
  (or
   (string-prefix-p "http://" url)
   (string-prefix-p "https://" url)))

;;;###autoload
(defun opener-try-open (url-or-file &optional force-buffer callback)
  "Try to open URL-OR-FILE appropriately.
This means a file-like URL in a buffer, any other URL in a browser
and a FILE as a normal file.
When FORCE-BUFFER non-nil, then actual URL is always opened in buffer.
CALLBACK gets executed in the not-url case."
  (if (opener-supported-url-scheme-p url-or-file)
    (if (or force-buffer (opener-file-like-url url-or-file))
        (opener-open-url-in-buffer url-or-file)
      (funcall opener-url-browser-function url-or-file))
    (when callback
      (funcall callback url-or-file))))

;;;###autoload
(defun opener-open-at-point ()
  "Opens URL or FILE at point."
  (interactive)
  (let ((url (ffap-url-at-point)))
    (opener-try-open url nil (lambda (_)
                               (find-file-at-point)))))

;;;###autoload
(defun opener-open (url-or-file force-buffer)
  "Open a URL-OR-FILE in buffer, with FORCE-BUFFER it opens URL in a buffer.
This means that it doesn't perform the file-like-url check to determine whether
to open url in buffer (= file-like-url t) or in a browser (= file-like-url
nil)."
  (interactive "sURL or FILE to open: \nP")
  (opener-try-open url-or-file force-buffer #'find-file))

(provide 'opener)
;;; opener.el ends here
