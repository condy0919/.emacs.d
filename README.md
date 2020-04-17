[![Build Status](https://github.com/condy0919/.emacs.d/workflows/CI/badge.svg)](https://github.com/condy0919/.emacs.d/actions)

个人**Emacs**配置
====

仿 [Centaur Emacs](https://github.com/seagle0128/.emacs.d) 的个人配置.

```bash
git clone https://github.com/condy0919/.emacs.d ~/.emacs.d
```

仅包含**C/C++/Rust/OCaml/Haskell**相关配置，且全线使用`lsp`。当前由于
`ocaml-lsp`十分难用，`haskell-ide-engine`水土不服，故这2个语言没有采用`lsp`。

保持着尽量使用`Emacs`自带功能的原则，能用自带的就用自带的。

# 基础配置

最基础的配置包含了那些在所有`mode`下都不会变更的配置，包含了：

| 包名       | 功能                                                 |
|------------|------------------------------------------------------|
| align      | `align-regexp`可以自动对齐选择的符号                 |
| autorevert | 当文本被其他编辑器修改后，可自动更新                 |
| delsel     | 选中文本可以直接覆盖着写，一般编辑器都默认开这个功能 |
| hl-line    | 高亮当前行                                           |
| paren      | 高亮匹配的括号                                       |
| saveplace  | 自动记录上次打开文件的位置                           |
| simple     | 在`modeline`里显示行号、列号以及当前文本的大小     |
| so-long    | 打开长行的文件不再痛苦 (`Emacs` 27+ 自带)            |

而这几个包也是`Emacs`自带的。

为了保持界面的整洁，禁用了菜单栏、工具栏和滚动条。

在跳转之后会闪烁一下当前行，这样就比较容易知道当前光标在哪里了。这个功能也是基于
自带的`pulse`。

# 插件配置、升级

选用`use-package`来管理插件。对于`elpa`, `melpa`里没有的包，使用`straight.el`辅
助下载。

而自动升级选择了`auto-package-update`。

# 界面

使用了`doom-themes`和`doom-modeline`，简直惊艳！

# 趁手的工具

`which-key`, `rg`是比较常用的工具。不过说实话，最近在使用`projectile`之后`fzf`也
用不着了，因为大多数场景都是在一个项目内跳转。由于有`counsel-projectile`的加成，
在原有`projectile`的基础上又添加了许多`ivy action`，更一步提升了便捷性。

`avy`用来代替`vim-easymotion`。而且`avy`还提供了`goto-line`的功能，这下都不用开
`relative line number`来`8k` `9j`这样跳了。

自然`ivy`,`counsel`是要上的，补全功能太好用了。没有`counsel`加持的`M-x`根本无法
让人按下去。这里没有使用`swiper`是因为它下方占用空间过大(继承于`ivy`的设置)，搜
索时肯定是比较在意上下文，而一个`swiper`就占用了`ivy-height`行就显得有点奢侈。而
自带的`isearch`在稍加设置之后，效果也还可以接受。当`evil-search-module`设置成
`isearch`后，也可以使用相同的快捷键来触发`ivy-occur`。再加上`ivy-occur`可以与
`wgrep`配合，将原来的「搜索、打开对应文件、修改」变成了「搜索、修改」。

`vterm`作为一个与原生终端更加接近的终端模拟器，单就外观来看已经比`Emacs`自带的
`eshell`好看。再加上`shell-pop`的辅助，美观又实用的`terminal`模拟器就出现了。

`Emacs`下的`markdown-mode`让人惊艳，突然觉得写文档也会这么快乐。与之相辅相成的还
有`separedit`,让人在代码里写`documentation comments`不再烦恼。

从`neovim`迁移过来的我，自然是常开`evil-mode`，相关的`evil`套件有:

- evil-leader
- evil-nerd-commenter
- evil-surround
- evil-magit

# 通用开发设置

- 显示行末空白字符
- 高亮**TODO** **FIXME**等关键字
- `dumb-jump`作为`lsp-find-defition`失败后的备份手段
- `magit`作为`git`客户端
- `hideshow`来显示/隐藏结构化的代码块，如 "{ }" 函数体等
- `rmsbolt`作为一个本地的 **Compiler Explorer** 相比于`godbolt`友好一点
- `quickrun`作为一个能够执行部分区域内的代码块，方便快速验证函数功能

# prog-mode

## cc-mode

- clangd `lsp-mode`
- 禁用了`flycheck`，因为`gcc/clang/cppcheck`的`checker`无法正确包含头文件的路径

## rust-mode

- rls `lsp-mode` 默认

## ocaml-mode

- 启用 `merlin` 作为补全后端

## haskell-mode

- 使用 `dante`

# 个性化

- 自己博客文章的查找、新建
- 使用`tempo`来插入`SDPX`形式的`license`头部
- 将常用的功能键绑定在`leader`键上
