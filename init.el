;;; init.el --- The main entry for emacs
;;; Commentary:
;;; Code:

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/"))

(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq debug-on-error t)

(add-to-list 'load-path (expand-file-name "plugins" user-emacs-directory))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(use-package init-base)
(use-package init-org)
(use-package init-ui)
(use-package init-misc)
(use-package init-evil)
(use-package init-company)
(use-package init-lsp)
(use-package init-git)
(use-package init-dev)

(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)
;;; init.el ends here
