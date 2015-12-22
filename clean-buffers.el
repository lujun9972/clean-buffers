;;; clean-buffers.el --- clean useless buffers

;; Copyright (C) 2004-2015 Free Software Foundation, Inc.

;; Author: DarkSun <lujun9972@gmail.com>
;; Created: 2015-12-22
;; Version: 0.1
;; Keywords: convenience, usability, buffers
;; Package-Requires: ((cl-lib "0.5"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Source code
;;
;; clean-buffers's code can be found here:
;;   http://github.com/lujun9972/clean-buffers

;;; Commentary:

;; clean-buffers is a little tool that used to clean useless buffers

;; Quick start:

;; config `useless-buffer-names' or `useless-buffer-time-out' and then
;; execute the following commands:
;; `cb-kill-useless-buffers' to clean useless buffers
;; or `cb-turn-on-auto-clean-buffers' to clean useless buffers automatically

;;; Code:

(require 'cl-lib)

(defun cb--buffer-active-p(buffer)
  "is the BUFFER already show in some window"
  (get-buffer-window buffer t))

(defun cb--buffer-process-holding-p (buffer)
  "is the BUFFER holding a process"
  (get-buffer-process buffer))

(defgroup clean-buffers nil
  "clean useless buffers"
  :prefix "cb-"
  :group 'convenience)

(defcustom cb-kill-active-buffer nil
  "clean active buffer or not"
  :type '(boolean)
  :group 'clean-buffers)

(defcustom cb-kill-proces-holding-buffer nil
  "clean process-holding buffer or not"
  :type '(boolean)
  :group 'clean-buffers)

(defcustom cb-judge-useless-buffer-functions '(cb-judge-useless-buffer-by-time cb-judge-useless-buffer-by-name)
  "function list which used to determine a buffer is useless or not 

the function will take a buffer as the only argument and should return non-nil when the buffer is a useless buffer."

  :group 'clean-buffers
  :type '(repeat function))

(defcustom cb-useless-buffer-time-out (* 7 24 3600)
  "used in `cb-judge-useless-buffer-by-time'"
  :group 'clean-buffers
  :type '(integer))

(defun cb-judge-useless-buffer-by-time (buffer)
  "buffer which did not displayed for specify time considered to be useless

the expire time is determined by `cb-useless-buffer-time-out'"
  (let (now buffer-last-display-time)
	(setq now (float-time (current-time)))
	(setq buffer-last-display-time (float-time (buffer-local-value 'buffer-display-time (get-buffer buffer))))
	(> (- now buffer-last-display-time) cb-useless-buffer-time-out)))

(defcustom cb-useless-buffer-names 
	'("*Buffer List*" "*Backtrace*" "*Apropos*" "*Completions*" "*Help*" "\\.~master~" "\\*vc-dir\\*" "\\*tramp\/.+\\*"  "\\*vc-git.+\\*")
	"useless buffer list"
	:group 'clean-buffers
	:type '(repeat regexp))

(defun cb-judge-useless-buffer-by-name (buffer)
  ""
  (cl-some (lambda (reg) (string-match reg buffer)) cb-useless-buffer-names))

(defcustom cb-useful-buffer-names 
	'("*Tree*")
	"useful buffer list"
	:group 'clean-buffers
	:type '(repeat regexp))

(defun cb--useless-buffer-p (buffer)
  "use functions in `cb-judge-useless-buffer-functions' to determine the BUFFER is a useless buffer or not"
  (when (bufferp buffer)
	(setq buffer (buffer-name buffer)))
  (and (not (cl-some (lambda (reg) (string-match reg buffer)) cb-useful-buffer-names))
	   (cl-some (lambda (fn) (funcall fn buffer)) cb-judge-useless-buffer-functions)))

(defun cb--kill-useless-buffer(buffer &optional kill-active kill-process-holding)
  "kill the BUFFER if the BUFFER is a useless buffer"
  (unless (or (not (cb--useless-buffer-p buffer))
			  (and (not kill-active) (cb--buffer-active-p buffer))
			  (and (not kill-process-holding) (cb--buffer-process-holding-p buffer)))
	(kill-buffer buffer)))

;;;###autoload
(defun cb-kill-useless-buffers()
  "clean all useless buffer"
  (interactive)
  (dolist (buffer (buffer-list))
	(cb--kill-useless-buffer buffer cb-kill-active-buffer cb-kill-proces-holding-buffer)))

(defcustom cb-auto-clean-interval 10
  "clean useless buffers interval"
  :type '(integer)
  :group 'clean-buffers)

(defvar cb-auto-clean-timer nil)

;;;###autoload
(defun cb-turn-off-auto-clean-buffers ()
  (interactive)
  (when (timerp cb-auto-clean-timer)
    (cancel-timer cb-auto-clean-timer)))

;;;###autoload
(defun cb-turn-on-auto-clean-buffers ()
  (interactive)
  (cb-turn-off-auto-clean-buffers)
  (setq cb-auto-clean-timer (run-with-timer 0 cb-auto-clean-interval #'cb-kill-useless-buffers)))

(provide 'clean-buffers)

;;; clean-buffers.el ends here
