;;; init-sql.el --- sql -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(use-package sql
  :ensure nil
  :mode ("\\.sql\\'" . sql-mode)
  :hook (sql-interactive-mode . toggle-truncate-lines))

(provide 'init-sql)

;;; init-sql.el ends here
