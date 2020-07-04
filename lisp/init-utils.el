;;; init-utils.el --- Utilities -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;;;###autoload
(defun ydcv-dwim ()
  "Call `ydcv' with active region or current symbol."
  (interactive)
  (let ((max-mini-window-height 0))
    (if (use-region-p)
        (shell-command-on-region (region-beginning) (region-end) "ydcv")
      (shell-command (format "ydcv %s" (word-at-point))))))

(provide 'init-utils)

;;; init-utils.el ends here
