;;; init-gpt.el --- GPT changes the world -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(use-package gptel
  :ensure t
  :hook (gptel-mode . gptel-highlight-mode)
  :bind (:map gptel-mode-map
         ("C-c C-g" . gptel-abort))
  :config
  (setq gptel-backend (gptel-make-deepseek "DeepSeek"
                        :stream t
                        :key 'gptel-api-key-from-auth-source))

  (gptel-make-preset 'gpt4coding
    :description "A preset optimized for coding tasks"
    :backend "DeepSeek"
    :model 'deepseek-reasoner
    :system "You are an expert coding assistant. Your role is to provide high-quality code solutions, refactorings, and explanations."
    :tools nil)
  :custom
  (gptel-default-mode 'markdown-mode)
  (gptel-model 'deepseek-reasoner))

(provide 'init-gpt)
;;; init-gpt.el ends here
