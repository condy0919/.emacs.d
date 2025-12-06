;;; init-gpt.el --- GPT changes the world -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(use-package gptel
  :ensure t
  :hook (gptel-mode . gptel-highlight-mode)
  :config
  (defvar gptel--deepseek
    (gptel-make-deepseek "DeepSeek"
      :stream t
      :key 'gptel-api-key-from-auth-source))
  :custom
  (gptel-model 'deepseek-reasoner)
  (gptel-backend gptel--deepseek))

(provide 'init-gpt)
;;; init-gpt.el ends here
