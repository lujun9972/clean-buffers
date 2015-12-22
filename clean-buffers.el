(defun cb--buffer-active-p(buffer)
  "判断buffer是否有在某个window中显示"
  (get-buffer-window buffer t))

(defun cb--buffer-process-holding-p (buffer)
  "判断buffer是否被某个process所持有"
  (get-buffer-process buffer))

(defgroup clean-buffers nil
  "定时清理无用的buffer")

(defcustom cb-kill-active-buffer nil
  "自动清理时是否清理active状态的buffer"
  :type '(boolean)
  :group 'clean-buffers)

(defcustom cb-kill-proces-holding-buffer nil
  "自动清理时是否清理被process持有的buffer"
  :type '(boolean)
  :group 'clean-buffers)

(defcustom cb-judge-useless-buffer-functions '(cb-judge-useless-buffer-by-time cb-judge-useless-buffer-by-name)
  "用于判断buffer是否为无用buffer的函数列表

用于判断buffer的函数会接收buffer作为唯一参数,并在buffer为无用时返回非nil"

  :group 'clean-buffers
  :type '(repeat function))

(defcustom cb-useless-buffer-time-out (* 7 24 3600)
  "cb-judge-useless-buffer-by-time中,超过该时长未显示的buffer被认为是无用的buffer,单位为秒"
  :group 'clean-buffers
  :type '(integer))

(defun cb-judge-useless-buffer-by-time (buffer)
  "超过一定时间未显示的buffer被认为是无用的

超时时间由`cb-useless-buffer-time-out'决定"
  (let (now buffer-last-display-time)
	(setq now (float-time (current-time)))
	(setq buffer-last-display-time (float-time (buffer-local-value 'buffer-display-time (get-buffer buffer))))
	(> (- now buffer-last-display-time) cb-useless-buffer-time-out)))

(defcustom cb-useless-buffer-names 
	'("*Buffer List*" "*Backtrace*" "*Apropos*" "*Completions*" "*Help*" "\\.~master~" "\\*vc-dir\\*" "\\*tramp\/.+\\*"  "\\*vc-git.+\\*")
	"无用buffer的名称列表"
	:group 'clean-buffers
	:type '(repeat regexp))

(defun cb-judge-useless-buffer-by-name (buffer)
  ""
  (some (lambda (reg) (string-match reg buffer)) cb-useless-buffer-names))

(defcustom cb-useful-buffer-names 
	'("*Tree*")
	"有用buffer的名称列表"
	:group 'clean-buffers
	:type '(repeat regexp))

(defun cb--useless-buffer-p (buffer)
  "使用cb-judge-useless-buffer-functions中的函数判断buffer是否为无用的buffer

匹配useful-buffer-name的buffer不会被清理"
  (when (bufferp buffer)
	(setq buffer (buffer-name buffer)))
  (and (not (some (lambda (reg) (string-match reg buffer)) cb-useful-buffer-names))
	   (some (lambda (fn) (funcall fn buffer)) cb-judge-useless-buffer-functions)))

(defun cb--kill-useless-buffer(buffer &optional kill-active kill-process-holding)
  "若buffer为无用的buffer,则kill之"
  (unless (or (not (cb--useless-buffer-p buffer))
			  (and (not kill-active) (cb--buffer-active-p buffer))
			  (and (not kill-process-holding) (cb--buffer-process-holding-p buffer)))
	(kill-buffer buffer)))

(defun cb-kill-useless-buffers()
  "清理所有的无用buffer"
  (interactive)
  (dolist (buffer (buffer-list))
	(cb--kill-useless-buffer buffer cb-kill-active-buffer cb-kill-proces-holding-buffer)))

(provide 'clean-buffers)


