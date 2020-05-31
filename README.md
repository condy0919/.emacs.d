<h3 align="center">A fast and incredible Emacs config</h3>

<p align="center">
  <img src="https://upload.wikimedia.org/wikipedia/commons/thumb/0/08/EmacsIcon.svg/120px-EmacsIcon.svg.png" />
</p>

<div align="center">

[![Build Status](https://github.com/condy0919/.emacs.d/workflows/CI/badge.svg)](https://github.com/condy0919/.emacs.d/actions)
[![License](http://img.shields.io/:license-gpl3-blue.svg)](LICENSE)
![Supports Emacs 26-28.x](https://img.shields.io/badge/Supports-Emacs_26.3_--_28.x-blueviolet.svg?style=flat-square&logo=GNU%20Emacs&logoColor=white)

</div>

个人**Emacs**配置
====

仿 [Centaur Emacs](https://github.com/seagle0128/.emacs.d) 的个人配置.

```bash
git clone --depth 1 https://github.com/condy0919/.emacs.d ~/.emacs.d
```

仅包含**C/C++/Rust/OCaml/Haskell**相关配置，且全线使用`lsp`。当前由于
`haskell-ide-engine`水土不服，故`haskell`没有采用`lsp`。

保持着尽量使用`Emacs`自带功能的原则，能用自带的就用自带的。

# 需要的依赖

- `hunspell` 拼写检查，目前仅在`git-commit-mode`下启用
- `languagetool` 更好的拼写检查、语法纠错工具
- `rg` 更快的`grep`
- `pandoc` 文本转换工具，`markdown-mode`渲染需要
- `fd` 更现代的 `find`
- `cmake` 编译`vterm`的模块、`c++`项目的构建工具
- `git` 这个就不用说了吧？
- `gcc` 这个就不用说了吧？

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
助下载。`straight.el`在自举过程中会连接`githubusercontent.com`这个域名，此域名在
国内访问几乎不可达，建议`bypass`之。

而自动升级选择了`auto-package-update`这个包。如果需要更新，`M-x`运行一下
`auto-package-update-now`即可。需要注意的是，更新是同步的。

# 界面

使用了`doom-themes`和`doom-modeline`，简直惊艳！`doom-one`的界面非常好看！

# 趁手的工具

`which-key`,`rg`是比较常用的工具。更有`projectile`管理项目，让项目编译、测试、
运行变得更加方便。而且还有`counsel-projectile`的加成，在原有`projectile`的基础上
又添加了许多`ivy action`，更一步提升了便捷性。

`avy`用来代替`vim-easymotion`。而且`avy`还提供了`goto-line`的功能，这下都不用开
`relative line number`来`8k` `9j`这样跳了。

自然`ivy`,`counsel`是要上的，补全功能太好用了。没有`counsel`加持的`M-x`根本无法
让人按下去。这里推荐尽量使用`isearch`，因为`swiper`下方占用空间过大(继承于
`ivy`的设置)，搜索时肯定是比较在意上下文。而自带的`isearch`在稍加设置之后，效果
也还可以接受。当`evil-search-module`设置成`isearch`后，也可以使用相同的快捷键来
触发`ivy-occur`。再加上`ivy-occur`可以与`wgrep`配合，将原来的「搜索、打开对应文
件、修改」变成了「搜索、修改」。

`vterm`作为一个与原生终端更加接近的终端模拟器，单就外观来看已经比`Emacs`自带的
`eshell`好看。再加上`shell-pop`的辅助，美观又实用的`terminal`模拟器就出现了。

`Emacs`下的`markdown-mode`让人惊艳，突然觉得写文档也会这么快乐。与之相辅相成的还
有`separedit`,让人在代码里写`documentation comments`不再烦恼。

从`neovim`迁移过来的我，自然是常开`evil-mode`，相关的`evil`套件有:

- evil-leader
- evil-collection
- evil-nerd-commenter
- evil-surround
- evil-magit

# 按键绑定

## evil-mode

`normal`状态下增加了如下键绑定:

| key           | function                                             |
|---------------|------------------------------------------------------|
| <kbd>gs</kbd> | `evil-avy-goto-char-timer` 来跳转到目标字符          |
| <kbd>go</kbd> | `evil-avy-goto-word-or-subword-1` 来跳转至目标单词处 |
| <kbd>gl</kbd> | `evil-avy-goto-line` 来跳转到对应行                  |

`avy`真乃神器也！

同时，因为开启了`evil-collection-want-unimpaired-p` (由`evil-collection`提供) 而
获得了如下键绑定:

| key           | function                                                         |
|---------------|------------------------------------------------------------------|
| <kbd>[b</kbd> | `previous-buffer` 切换至上一个 `buffer`                          |
| <kbd>]b</kbd> | `next-buffer` 切换至下一个 `buffer`                              |
| <kbd>[l</kbd> | `evil-collection-unimpaired-previous-error` 上一个错误           |
| <kbd>]l</kbd> | `evil-collection-unimpaired-next-error` 下一个错误               |
| <kbd>[ </kbd> | `evil-collection-unimpaired-insert-space-above` 在上方插入一空行 |
| <kbd>] </kbd> | `evil-collection-unimpaired-insert-space-below` 在下方插入一空行 |

本配置里使用`hideshow`来`fold`代码块。由于`hideshow`本身提供的快捷键非常长，非常
推荐使用`evil-mode`在`normal`状态下定义的键绑定。

| key           | function                                |
|---------------|-----------------------------------------|
| <kbd>zm</kbd> | `evil-close-folds`隐藏所有代码块        |
| <kbd>zr</kbd> | `evil-open-folds`显示所有被隐藏的代码块 |
| <kbd>zo</kbd> | `evil-open-fold`隐藏当前代码块          |
| <kbd>zc</kbd> | `evil-close-fold`显示当前被隐藏的代码块 |

## Emacs

| key                | function                                                                      |
|--------------------|-------------------------------------------------------------------------------|
| <kbd>M-;</kbd>     | `evilnc-comment-or-uncomment-lines` 注释与反注释                              |
| <kbd>C-c '</kbd>   | 通过`separedit`在注释中快乐地写代码                                           |
| <kbd>C-c x</kbd>   | 调用`quickrun`来运行当前`buffer`内的代码。`eval`快人一步！                    |
| <kbd>M-=</kbd>     | 在下方弹出一个`vterm`终端                                                     |
| <kbd>C-c p</kbd>   | `projectile`调用前缀，方便地在项目内跳转、编译等其他功能                      |
| <kbd>C-c t o</kbd> | `hl-todo-occur`查找当前`buffer`内的**TODO**/**FIXME**等关键字                 |
| <kbd>C-c t p</kbd> | 上一个高亮的关键字                                                            |
| <kbd>C-c t n</kbd> | 下一个高亮的关键字                                                            |
| <kbd>C-x g</kbd>   | 呼出 `magit`                                                                  |
| <kbd>C-M-;</kbd>   | 在`git-commit`时会有`flyspell`检查单词是否错误，通过此按键自动修正            |
| <kbd>M-o</kbd>     | 原生`C-x o`来切换`window`有点反人类，绑定在单键上就可以快速的切换至其他窗口了 |
| <kbd>C-c [</kbd>   | 调用`align-regexp`提供以一个对齐符号的功能, <kbd>C-c ]</kbd> 也有同样效果     |
| <kbd>C-c i l</kbd> | 方便地插入`SPDX`形式的`license`头部                                           |

因为[projectile](https://github.com/bbatsov/projectile)比较常用，把它单独拿出来
说。本配置中还使用了`counsel-projectile`来令`projectile`更加方便。

| key                  | function                                                             |
|----------------------|----------------------------------------------------------------------|
| <kbd>C-c p f</kbd>   | `projectile-find-file`在项目内查找其他文件                           |
| <kbd>C-c p b</kbd>   | `projectile-switch-to-buffer`切换至其他`buffer`(限定在本`project`下) |
| <kbd>C-c p C</kbd>   | `projectile-configure-project`配置当前项目                           |
| <kbd>C-c p c</kbd>   | `projectile-compile-project`编译当前项目                             |
| <kbd>C-c p u</kbd>   | `projectile-run-project`运行当前项目                                 |
| <kbd>C-c p P</kbd>   | `projectile-test-project`测试当前项目                                |
| <kbd>C-c p p</kbd>   | `projectile-switch-project`切换至其他项目                            |
| <kbd>C-c p s r</kbd> | `projectile-ripgrep`使用`ripgrep`来搜索当前项目内的文本。此功能依赖`ripgrep`这个包，不过在`counsel-projectile`加持下被重新`remap`过。              |

基于同样的理由，把`flycheck`单独拎了出来。

| key                | function                                  |
|--------------------|-------------------------------------------|
| <kbd>C-c ! l</kbd> | `flycheck-list-errors`列出所有`lint`错误  |
| <kbd>C-c ! n</kbd> | `flycheck-next-error`下一个`lint`错误     |
| <kbd>C-c ! p</kbd> | `flycheck-previous-error`上一下`lint`错误 |

更详细的按键绑定请直接看[代码](lisp/init-evil.el). :-)

# 通用开发设置

- 显示行末空白字符
- 高亮**TODO** **FIXME**等关键字
- `dumb-jump`作为`lsp-find-definition`失败后的备份手段
- `magit`作为`git`客户端
- `hideshow`来显示/隐藏结构化的代码块，如 "{ }" 函数体等
- `rmsbolt`作为一个本地的 **Compiler Explorer** 相比于`godbolt`友好一点
- `ispell`拼写检查器, `evil`用户可以快速通过<kbd>zm</kbd> (`ispell-word`) 来检查
- `flyspell`拼写检查器，仅在`magit`写提交信息时启用
- `quickrun`作为一个能够执行部分区域内的代码块，方便快速验证函数功能

# prog-mode

## cc-mode

使用`lsp-mode`作为补全、符号查找的工具，默认后端使用`clangd`，一般发行版的源里都
会有对应的包。如果想使用[ccls][ccls]，可以`customize`对应的变量:

``` emacs-lisp
(setq lsp-clients-clangd-executable "ccls"
      lsp-clients-clangd-args nil)
```

如果想使用`ccls`的`lsp`扩展功能，需要安装[ccls][emacs-ccls]扩展。

禁用了`flycheck`自带的3个`checker`(分别为`c/c++-clang`, `c/c++-cppcheck`,
`c/c++-gcc`)，因为它们都无法正确包含自定义的头文件路径。

此外，

- `c++-mode`启用了[modern-cpp-font-lock][modern-cpp-font-lock]
- `cmake-mode`可使用`company-mode`进行符号补全

## rust-mode

使用`lsp-mode`作为补全、符号查找的工具，默认后端使用`rls`,一般发行版会把它直接跟
`rust`绑在一起，也可以使用`rustup`来安装。对于`rust-analyzer`用户而言，通过设置

``` emacs-lisp
(setq lsp-rust-server 'rust-analyzer)
```

来切换至`rust-analyzer`。

当然，

- `rust-mode`开启了保存时格式化文件，需要确保`rustfmt`二进制包存在
- 使用了[cargo][cargo]来提供深度集成化的`cargo`命令

## ocaml-mode

使用`lsp-mode`作为补全、符号查找的工具。在`Arch Linux`上，可以使用
[ocaml-lsp-git][ocaml-lsp-git]这个包。

由于`ocaml-lsp-git`目前**只**实现了`lsp-format-buffer`且额外依赖`ocamlformat`。

所以这里额外使用了[ocp-indent][ocp-indent]，通过`ocp-indent-region`,
`ocp-indent-buffer`来提供格式化代码的功能。

## haskell-mode

- 使用 `dante`

# 个性化

- 自己博客文章的查找、新建
- 插入`SPDX`形式的`license`头功能已独立[license.el](https://github.com/condy0919/license.el)
- 将常用的功能键绑定在`leader`键上

# 截图

![dashboard](https://user-images.githubusercontent.com/4024656/83343041-46cbc480-a328-11ea-800d-6cd88540186e.png
"Dashboard")

![magit_markdown](https://user-images.githubusercontent.com/4024656/83343152-30723880-a329-11ea-90ff-ec5f3b7d504a.png
"Magit and markdown")

![cpp_company](https://user-images.githubusercontent.com/4024656/83343182-965ec000-a329-11ea-862c-a0b25305a1b4.png
"cc-mode and company")

[ccls]: https://github.com/MaskRay/ccls/
[cargo]: https://melpa.org/#/cargo
[emacs-ccls]: https://melpa.org/#/ccls
[ocp-indent]: https://melpa.org/#/ocp-indent
[modern-cpp-font-lock]: https://github.com/ludwigpacifici/modern-cpp-font-lock/
[ocaml-lsp-git]: https://aur.archlinux.org/packages/ocaml-lsp-git
