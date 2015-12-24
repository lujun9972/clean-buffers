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
;; `clean-buffer-kill-useless-buffers' to clean useless buffers
;; or `clean-buffer-turn-on-auto-clean-buffers' to clean useless buffers automatically

;;; Code:

(require 'cl-lib)

(defun clean-buffer--buffer-active-p(buffer)
  "is the BUFFER already show in some window"
  (get-buffer-window buffer t))

(defun clean-buffer--buffer-process-holding-p (buffer)
  "is the BUFFER holding a process"
  (get-buffer-process buffer))

(defgroup clean-buffers nil
  "clean useless buffers"
  :prefix "clean-buffer-"
  :group 'convenience)

(defcustom clean-buffer-kill-active-buffer nil
  "clean active buffer or not"
  :type '(boolean)
  :group 'clean-buffers)

(defcustom clean-buffer-kill-proces-holding-buffer nil
  "clean process-holding buffer or not"
  :type '(boolean)
  :group 'clean-buffers)

(defcustom clean-buffer-judge-useless-buffer-functions '(clean-buffer-judge-useless-buffer-by-time clean-buffer-judge-useless-buffer-by-name)
  "function list which used to determine a buffer is useless or not 

the function will take a buffer as the only argument and should return non-nil when the buffer is a useless buffer."

  :group 'clean-buffers
  :type '(repeat function))

(defcustom clean-buffer-useless-buffer-time-out (* 7 24 3600)
  "used in `clean-buffer-judge-useless-buffer-by-time'"
  :group 'clean-buffers
  :type '(integer))

(defun clean-buffer-judge-useless-buffer-by-time (buffer)
  "buffer which did not displayed for specify time considered to be useless

the expire time is determined by `clean-buffer-useless-buffer-time-out'"
  (let (now buffer-last-display-time)
	(setq now (float-time (current-time)))
	(setq buffer-last-display-time (float-time (buffer-local-value 'buffer-display-time (get-buffer buffer))))
	(> (- now buffer-last-display-time) clean-buffer-useless-buffer-time-out)))

(defcustom clean-buffer-useless-buffer-names 
	'("*Buffer List*" "*Backtrace*" "*Apropos*" "*Completions*" "*Help*" "\\.~master~" "\\*vc-dir\\*" "\\*tramp\/.+\\*"  "\\*vc-git.+\\*")
	"useless buffer list"
	:group 'clean-buffers
	:type '(repeat regexp))

(defun clean-buffer-judge-useless-buffer-by-name (buffer)
  ""
  (cl-some (lambda (reg) (string-match reg buffer)) clean-buffer-useless-buffer-names))

(defcustom clean-buffer-useful-buffer-names 
	'("*Tree*")
	"useful buffer list"
	:group 'clean-buffers
	:type '(repeat regexp))

(defun clean-buffer--useless-buffer-p (buffer)
  "use functions in `clean-buffer-judge-useless-buffer-functions' to determine the BUFFER is a useless buffer or not"
  (when (bufferp buffer)
	(setq buffer (buffer-name buffer)))
  (and (not (cl-some (lambda (reg) (string-match reg buffer)) clean-buffer-useful-buffer-names))
	   (cl-some (lambda (fn) (funcall fn buffer)) clean-buffer-judge-useless-buffer-functions)))

(defun clean-buffer--kill-useless-buffer(buffer &optional kill-active kill-process-holding)
  "kill the BUFFER if the BUFFER is a useless buffer"
  (unless (or (not (clean-buffer--useless-buffer-p buffer))
			  (and (not kill-active) (clean-buffer--buffer-active-p buffer))
			  (and (not kill-process-holding) (clean-buffer--buffer-process-holding-p buffer)))
	(kill-buffer buffer)))

;;;###autoload
(defun clean-buffer-kill-useless-buffers()
  "clean all useless buffer"
  (interactive)
  (dolist (buffer (buffer-list))
	(clean-buffer--kill-useless-buffer buffer clean-buffer-kill-active-buffer clean-buffer-kill-proces-holding-buffer)))

(defcustom clean-buffer-auto-clean-interval 10
  "clean useless buffers interval"
  :type '(integer)
  :group 'clean-buffers)

(defvar clean-buffer-auto-clean-timer nil)

;;;###autoload
(defun clean-buffer-turn-off-auto-clean-buffers ()
  (interactive)
  (when (timerp clean-buffer-auto-clean-timer)
    (cancel-timer clean-buffer-auto-clean-timer)))

;;;###autoload
(defun clean-buffer-turn-on-auto-clean-buffers ()
  (interactive)
  (clean-buffer-turn-off-auto-clean-buffers)
  (setq clean-buffer-auto-clean-timer (run-with-timer 0 clean-buffer-auto-clean-interval #'clean-buffer-kill-useless-buffers)))

(provide 'clean-buffers)

;;; clean-buffers.el ends here
