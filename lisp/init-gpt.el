;;; init-gpt.el --- GPT changes the world -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(use-package gptel
  :ensure t
  :hook ((gptel-mode . gptel-highlight-mode)
         (gptel-post-stream . gptel-auto-scroll))
  :bind (:map gptel-mode-map
         ("C-c C-g" . gptel-abort))
  :config
  (setq gptel-backend (gptel-make-deepseek "DeepSeek"
                        :stream t
                        :key 'gptel-api-key-from-auth-source))

  (gptel-make-preset 'thinking
    :description "Enable chain-of-thought reasoning"
    :request-params '(:think (:type "enabled")))

  (gptel-make-preset 'coding
    :description "A preset optimized for coding tasks"
    :system "You are an expert coding assistant. Your role is to provide high-quality code solutions, refactorings, and explanations."
    :tools nil)
  :custom
  (gptel-model 'deepseek-reasoner)
  (gptel-track-media t)
  (gptel-include-reasoning 'ignore)
  (gptel-highlight-methods (if (display-graphic-p) '(fringe) '(margin)))
  (gptel-default-mode 'org-mode)
  (gptel-org-branching-context t)
  (gptel-prompt-prefix-alist nil)
  (gptel-response-prefix-alist nil))

(use-package gptel
  :ensure nil
  :after magit
  :config
  (defconst gptel-commit-system-prompt
    "You are an expert programmer writing a good Git commit message.
You have carefully reviewed every file diff included in this commit.
Now, write the commit message using the Conventional Commits
specification. The first line should be no more than 68 characters.")

  (defun gptel-commit ()
    "Generate Git commit message with gptel"
    (interactive)
    (if-let* ((lines (magit-git-lines "diff" "--cached"))
              (changes (string-join lines "\n")))
      (let ((gptel-include-reasoning nil)
            (gptel-use-context 'system)
            (gptel-use-tools nil))
        (gptel-request changes
          :system gptel-commit-system-prompt
          :stream t))
      (message "You have no file staged"))))

(provide 'init-gpt)
;;; init-gpt.el ends here
