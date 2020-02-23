;;; init-web.el --- All about web -*- lexical-binding: t -*-

;;; Commentary:
;;
;;; Code:

(use-package css-mode
  :ensure nil
  :custom
  (css-indent-offset 2))

(use-package json-mode
  :ensure t
  :bind (:map json-mode-map
         ("C-c TAB" . json-mode-beautify)))

;; highlight #535353 stuff
(use-package rainbow-mode
  :ensure t
  :diminish rainbow-mode
  :hook ((web-mode html-mode css-mode) . rainbow-mode))

(use-package web-mode
  :ensure t
  :mode "\\.\\(phtml\\|php|[gj]sp\\|as[cp]x\\|erb\\|djhtml\\|html?\\|hbs\\|ejs\\|jade\\|swig\\|tm?pl\\|vue\\)$"
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2))

(provide 'init-web)
;;; init-web.el ends here
