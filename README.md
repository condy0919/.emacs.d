个人**emacs**配置
====

仿 [Centaur Emacs](https://github.com/seagle0128/.emacs.d) 的个人配置.

```bash
git clone https://github.com/condy0919/.emacs.d ~/.emacs.d
```

仅包含**C/C++/Rust/OCaml**相关配置，且全线使用 `lsp`

# 基础配置

最基础的配置包含了那些在所有 `mode` 下都不会变更的配置，包含了：

| 包名      | 功能                           |
|-----------|--------------------------------|
| paren     | 高亮匹配的括号                 |
| simple    | 在 `modeline` 里显示行号、列号 |
| saveplace | 自动记录上次打开文件的位置     |
| hl-line   | 高亮当前行                     |

而这 4 个包也是 `Emacs` 自带的。

为了保持界面的整洁，禁用了菜单栏、工具栏和滚动条。

# 趁手的工具

`which-key`, `rg`, `fzf` 是比较常用的工具。不过说实话，最近在使用 `projectile`
之后 `fzf` 用得也少了，因为大多数场景都是在一个项目内跳转。

`avy` 用来代替 `vim-easymotion` 。而且`avy` 还提供了 `goto-line` 的功能，这下都
不用开 `relative line number` 来 `8k` `9j` 这样跳了。

自然 `ivy`, `counsel` 是要上的，补全功能太好用了。没有`counsel` 加持的 `M-x` 根
本无法让人按下去。

`vterm` 作为一个与原生终端更加接近的终端模拟器，单就外观来看已经比 `Emacs` 自带
的 `eshell` 好看。

`Emacs` 下的 `markdown-mode` 让人惊艳，突然觉得写文档也会这么快乐。 与之相辅相成
的还有 `comment-edit`, 让人在代码里写 `comment documentation` 不再单调 :)

从 `neovim` 迁移过来的我，自然是常开 `evil-mode`，相关的 `evil` 套件有:

- evil-leader
- evil-nerd-commenter
- evil-surround
- evil-magit

# 通用开发设置

- 显示行末空白字符
- dumb-jump 作为 `lsp-find-defition` 失败后的备份手段
- magit 作为 `git` 客户端

# cc-mode

- clangd `lsp-mode`
- 禁用了 `flycheck`，因为 `gcc/clang/cppcheck` 的 `flychecker` 无法正确包含头文
  件的路径

# rust-mode

- rls `lsp-mode` 默认

# ocaml-mode

- 启用 `merlin` 作为补全后端
