;;; init-gpt.el --- GPT changes the world -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(use-package gptel
  :ensure t
  :commands gptel-commit
  :hook (gptel-mode . gptel-highlight-mode)
  :bind (:map gptel-mode-map
         ("C-c C-g" . gptel-abort))
  :config
  (declare-function magit-git-lines "magit-git" (&rest args))
  (declare-function gptel-request "gptel-request" (&optional prompt &rest args))

  (setq gptel-backend (gptel-make-deepseek "DeepSeek"
                        :stream t
                        :key 'gptel-api-key-from-auth-source))

  (gptel-make-preset 'coding
    :description "A preset optimized for coding tasks"
    :system "You are an expert coding assistant. Your role is to provide high-quality code solutions, refactorings, and explanations."
    :tools nil)

  (gptel-make-preset 'commit
    :description "Generate Git commit messages"
    :model 'deepseek-flash
    :system "You are an expert programmer writing a good Git commit message.
You have carefully reviewed every file diff included in this commit.
Now, write the commit message using the Conventional Commits
specification. The first line should be no more than 68 characters."
    :include-reasoning nil
    :use-context 'system
    :use-tools nil)

  (defun gptel-commit ()
    "Generate Git commit message with gptel."
    (interactive)
    (require 'magit-process)
    (if-let* ((lines (magit-git-lines "diff" "--cached"))
              (changes (string-join lines "\n")))
        (gptel-with-preset 'commit
          (gptel-request changes :stream t))
      (message "You have no file staged")))
  :custom
  (gptel-model 'deepseek-flash)
  (gptel-track-media t)
  (gptel-include-reasoning 'ignore)
  (gptel-default-mode 'org-mode)
  (gptel-org-branching-context t)
  (gptel-prompt-prefix-alist nil)
  (gptel-response-prefix-alist nil))

(provide 'init-gpt)
;;; init-gpt.el ends here
