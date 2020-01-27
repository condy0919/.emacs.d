个人**emacs**配置
====

仿 [Centaur Emacs](https://github.com/seagle0128/.emacs.d) 的个人配置.

```bash
git clone https://github.com/condy0919/.emacs.d ~/.emacs.d
```

仅包含**C/C++/Rust**相关配置，且全线使用 `lsp`

# 基础配置

最基础的配置包含了那些在所有 `mode` 下都不会变更的配置，包含了：

| 包名      | 功能                           |
|-----------|--------------------------------|
| paren     | 高亮匹配的括号                 |
| simple    | 在 `modeline` 里显示行号、列号 |
| saveplace | 自动记录上次打开文件的位置     |
| hl-line   | 高亮当前行                     |

而这 4 个包也是 `Emacs` 自带的。

为了保持界面的整洁，禁用的菜单栏、工具栏和滚动条。

# 趁手的工具

`which-key`, `rg`, `fzf` 是比较常用的工具。不过说实话，最近在使用 `projectile`
之后 `fzf` 用得也少了，因为大多数场景都是在一个项目内跳转。

`avy` 用来代替 `vim-easymotion` 。而且`avy` 还提供了 `goto-line` 的功能，这下都
不用开 `relative line number` 来 `8k` `9j` 这样跳了。

自然 `ivy`, `counsel` 和 `swiper` 3 件套是要上的，补全功能太好用了。没有
`counsel` 加持的 `M-x` 根本无法让人按下去。

`Emacs` 下的 `markdown-mode` 让人惊艳，突然觉得写文档也会这么快乐。 与之相辅相成
的还有 `comment-edit`, 让人在代码里写 `comment doc` 不再单调 :)

# cc-mode

- clangd `lsp-mode`
- 禁用了 `flycheck`

# rust-mode

- rls `lsp-mode` 默认
