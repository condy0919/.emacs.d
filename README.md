<h3 align="center">A fast and incredible Emacs config</h3>

<p align="center">
  <img src="https://upload.wikimedia.org/wikipedia/commons/thumb/0/08/EmacsIcon.svg/120px-EmacsIcon.svg.png" />
</p>

<div align="center">

[![Build Status](https://github.com/condy0919/.emacs.d/workflows/CI/badge.svg)](https://github.com/condy0919/.emacs.d/actions)
[![License](http://img.shields.io/:license-gpl3-blue.svg)](LICENSE)
![Supports Emacs 27.1-28.x](https://img.shields.io/badge/Supports-Emacs_27.1_--_28.x-blueviolet.svg?style=flat-square&logo=GNU%20Emacs&logoColor=white)

</div>

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [个人**Emacs**配置](#个人emacs配置)
- [需要的依赖](#需要的依赖)
- [基础配置](#基础配置)
- [插件配置、升级](#插件配置升级)
- [界面](#界面)
- [趁手的工具](#趁手的工具)
- [按键绑定](#按键绑定)
    - [evil-mode](#evil-mode)
    - [Emacs](#emacs)
- [通用开发设置](#通用开发设置)
- [prog-mode](#prog-mode)
    - [cc-mode](#cc-mode)
    - [rust-mode](#rust-mode)
    - [ocaml-mode](#ocaml-mode)
    - [haskell-mode](#haskell-mode)
- [截图](#截图)
- [FAQ](#faq)
- [其他](#其他)

<!-- markdown-toc end -->

个人**Emacs**配置
====

![overview](https://user-images.githubusercontent.com/4024656/83358470-63a6dd00-a3a6-11ea-8489-f86b08b48d13.png
"Emacs Overview")

仿 [Centaur Emacs](https://github.com/seagle0128/.emacs.d) 的个人配置.

```bash
git clone --depth 1 https://github.com/condy0919/.emacs.d ~/.emacs.d
```

仅包含**C/C++/Rust/OCaml/Haskell**相关配置，且全线使用`lsp`。当前由于`haskell-ide-engine`水土不服，故`haskell`没有采用`lsp`。

保持着尽量使用`Emacs`自带功能的原则，能用自带的就用自带的。

# 需要的依赖

- `hunspell` 拼写检查，目前仅在`git-commit-mode`下启用
- `rg` 更快的`grep`
- `pandoc` (optional) 文本转换工具，`markdown-mode`渲染需要
- `markdown` (optional) 文本转换工具，`markdown-mode`渲染需要
- `cmake` `c++`项目的构建工具
- `git` 这个就不用说了吧？
- `gcc` 这个就不用说了吧？
- `fd` (optional) 更现代的 `find`, `projectile` 会自动检测

# 基础配置

最基础的配置包含了那些在所有`mode`下都不会变更的配置，包含了：

| 包名          | 功能                                                 |
|---------------|------------------------------------------------------|
| align         | `align-regexp`可以自动对齐选择的符号                 |
| appt          | 任务提醒，可以与`org-mode`结合                       |
| autorevert    | 当文本被其他编辑器修改后，可自动更新                 |
| delsel        | 选中文本可以直接覆盖着写，一般编辑器都默认开这个功能 |
| hippie-expand | 用来展开文本                                         |
| hl-line       | 高亮当前行                                           |
| newcomment    | 注释、反注释功能                                     |
| paren         | 高亮匹配的括号                                       |
| saveplace     | 自动记录上次打开文件的位置                           |
| simple        | 在`modeline`里显示行号、列号以及当前文本的大小       |
| so-long       | 打开长行的文件不再痛苦 (`Emacs` 27+ 自带)            |
| tab-bar       | 窗口布局管理 (`Emacs` 27+ 自带)                      |
| tramp         | 远程编辑就靠它                                       |

而这几个包也是`Emacs`自带的。

为了保持界面的整洁，禁用了菜单栏、工具栏和滚动条。

~~在跳转之后会闪烁一下当前行，这样就比较容易知道当前光标在哪里了。这个功能也是基于自带的pulse。~~ 太卡了，禁用了。

# 插件配置、升级

使用`package.el`(自带的)来安装包、`use-package`来管理配置。对于`elpa`, `melpa`里没有的包，使用`quelpa`辅助下载。

为什么我会从`straight.el`切换至`quelpa`呢？

主要是`straight.el`不支持单个文件的下载、配置，为了使用`llvm-mode.el`而 clone 整个 llvm repo 就得不尝失了吧。相关配置见[`init-cpp.el`](lisp/lang/init-cpp.el)内的`llvm-mode`配置项。另外由于`quelpa`与 `package-quickstart`冲突，`llvm-mode`和`tablegen-mode`需要用户自己执行对应的`quelpa`代码块来提前安装，而不是通过`use-package`自动检测、下载。不过因为`quelpa`安装过后的包也会在`~/.emacs.d/elpa/`里放一份，所以实际上也没差多少。

而自动升级选择了`auto-package-update`这个包。如果需要更新，<kbd>M-x auto-package-update-now</kbd> 即可。如果想要异步更新，则使用<kbd>M-x auto-package-update-now-async</kbd>。由于 `package-refresh-contents` 暂时未暴露 callback 接口所以无法与之前的包更新操作构成异步流，需要用户提前 <kbd>M-x package-refresh-contents</kbd>.

# 界面

使用了`doom-themes`和`doom-modeline`，简直惊艳！`doom-one`的界面非常好看！

# 趁手的工具

`rg`是比较常用的工具，更有`projectile`管理项目，让项目编译、测试、运行变得更加方便。

`avy`用来代替`vim-easymotion`。而且`avy`还提供了`goto-line`的功能，这下都不用开相对行号`8k` `9j`这样跳了。

以前是`ivy`用户，现在则是仅使用`vertico`, `embark`和`consult`了。

`Emacs`下的`org-mode`/`markdown-mode`让人惊艳，突然觉得写文档也会这么快乐。与之相辅相成的还有`separedit`，让人在代码里写`documentation comments`不再烦恼。

[valign][valign] 提供了像素级别的表格对齐，终于不用再靠西文半宽的字体了！

从`neovim`迁移过来的我，自然是常开`evil-mode`，相关的`evil`套件有:

- `evil`
- `evil-collection` (已包含 `evil-magit`)
- `evil-surround`

# 按键绑定

## evil-mode

开启了`evil-collection-want-unimpaired-p` (由`evil-collection`提供) 而获得了如下键绑定:

| key              | function                                                            |
|------------------|---------------------------------------------------------------------|
| <kbd>[b</kbd>    | `previous-buffer` 切换至上一个 `buffer`                             |
| <kbd>]b</kbd>    | `next-buffer` 切换至下一个 `buffer`                                 |
| <kbd>[e</kbd>    | `evil-collection-unimpaired-move-text-up` 将当前行移动至上一行      |
| <kbd>]e</kbd>    | `evil-collection-unimpaired-move-text-down` 将当前行移动至下一行    |
| <kbd>[l</kbd>    | `evil-collection-unimpaired-previous-error` 上一个错误              |
| <kbd>]l</kbd>    | `evil-collection-unimpaired-next-error` 下一个错误                  |
| <kbd>[ SPC</kbd> | `evil-collection-unimpaired-insert-newline-above` 在上方插入一空行  |
| <kbd>] SPC</kbd> | `evil-collection-unimpaired-insert-newline-below` 在下方插入一空行  |
| <kbd>[u</kbd>    | `evil-collection-unimpaired-url-encode` 对所选内容进行`url`参数编码 |
| <kbd>]u</kbd>    | `evil-collection-unimpaired-url-decode` 对所选内容进行`url`参数解码 |

此外，凭借 avy 模拟了 [evil-snipe][evil-snipe] 的 `s` 和 `f` 功能。

| key          | function                     |
|--------------|------------------------------|
| <kbd>s</kbd> | `evil-avy-goto-char-timer`   |
| <kbd>f</kbd> | `evil-avy-goto-char-in-line` |

本配置里使用`hideshow`来`fold`代码块。由于`hideshow`本身提供的快捷键非常长，非常推荐使用`evil-mode`在`normal`状态下定义的键绑定。

| key           | function                                            |
|---------------|-----------------------------------------------------|
| <kbd>zm</kbd> | `evil-close-folds`隐藏所有代码块                    |
| <kbd>zr</kbd> | `evil-open-folds`显示所有被隐藏的代码块             |
| <kbd>zo</kbd> | `evil-open-fold`隐藏当前代码块                      |
| <kbd>zO</kbd> | `evil-open-fold-rec`递归地隐藏当前以及之内的代码块  |
| <kbd>zc</kbd> | `evil-close-fold`显示当前被隐藏的代码块             |
| <kbd>zC</kbd> | `evil-close-fold-rec`递归地显示当前以及之内的代码块 |
| <kbd>za</kbd> | `evil-toggle-fold`来切换是否隐藏代码                |

与文件相关的`Leader`键绑定如下:

| key           | function                                                                    |
|---------------|-----------------------------------------------------------------------------|
| <kbd>ff</kbd> | `find-file`打开文件, <kbd>f.</kbd>有相同效果                                |
| <kbd>fF</kbd> | `find-file-other-window`同上，不过是在另一窗口打开, <kbd>f/</kbd>有相同效果 |
| <kbd>f/</kbd> | 同上                                                                        |
| <kbd>fD</kbd> | `+delete-current-file`删除当前文件                                        |
| <kbd>fC</kbd> | `+copy-current-file`拷贝当前文件至其他地方                                |
| <kbd>fy</kbd> | `+copy-current-filename`拷贝当前文件的绝对路径                            |
| <kbd>fR</kbd> | `+rename-current-file`重命名当前文件                                      |
| <kbd>fr</kbd> | `recentf-open-files`访问最近使用过的文件                                       |
| <kbd>fl</kbd> | `find-file-literally`采用朴素模式打开文件                                   |

与目录相关的`Leader`键绑定如下:

| key           | function                                            |
|---------------|-----------------------------------------------------|
| <kbd>dj</kbd> | `dired-jump`进入当前文件的目录                      |
| <kbd>dJ</kbd> | `dired-jump-other-window`同上，不过是在另一窗口打开 |

与`buffer`、`bookmark`相关的键绑定:

| key           | function                                                                              |
|---------------|---------------------------------------------------------------------------------------|
| <kbd>bb</kbd> | `switch-to-buffer`切换`buffer`                                                        |
| <kbd>bB</kbd> | `switch-to-buffer-other-window`同上，不过是在另一窗口打开                             |
| <kbd>bc</kbd> | `clone-indirect-buffer`将当前`buffer`克隆至另一`buffer`，它们可以使用不同`major-mode` |
| <kbd>bC</kbd> | `clone-indirect-buffer-other-window`同上，不过是在另一窗口打开                        |
| <kbd>bv</kbd> | `revert-buffer`重新读取当前`buffer`对应的文件                                         |
| <kbd>by</kbd> | `+copy-current-buffer-name`复制当前`buffer`的名字                                   |
| <kbd>bz</kbd> | `bury-buffer`退出当前`buffer`的显示，当前`buffer`未被 kill                            |

| key           | function                                                |
|---------------|---------------------------------------------------------|
| <kbd>bj</kbd> | `bookmark-jump`跳转至书签                               |
| <kbd>bJ</kbd> | `bookmark-jump-other-window`同上，不过是在另一窗口打开  |
| <kbd>bm</kbd> | `bookmark-set`设置书签                                  |
| <kbd>bM</kbd> | `bookmark-set-no-overwrite`同上，但是不会覆盖同名的书签 |
| <kbd>bd</kbd> | `bookmark-delete`删除书签                               |
| <kbd>bi</kbd> | `bookmark-insert`插入书签的内容                         |
| <kbd>bl</kbd> | `bookmark-bmenu-list`打开书签列表                       |
| <kbd>br</kbd> | `bookmark-rename`重命名书签                             |
| <kbd>bs</kbd> | `bookmark-save`保存书签                                 |
| <kbd>bw</kbd> | `bookmark-write`将书签保存至其他文件                    |

打开其他程序的`Leader`键绑定:

| key           | function                                                   |
|---------------|------------------------------------------------------------|
| <kbd>ot</kbd> | `ansi-term`打开`ansi-term`                              |
| <kbd>oe</kbd> | `eshell`打开`eshell`                                       |
| <kbd>os</kbd> | `shell`打开`shell`                                         |
| <kbd>ol</kbd> | `org-store-link`存储URL                                    |
| <kbd>oc</kbd> | `org-capture`随时记录一些想法、URL等                       |

打开一些看起来像是独立的应用:

| key           | function                     |
|---------------|------------------------------|
| <kbd>aa</kbd> | `org-agenda`日程             |
| <kbd>ac</kbd> | `calendar`日历               |
| <kbd>ag</kbd> | `gnus`查看新闻组             |
| <kbd>ai</kbd> | `rcirc`上 IRC                |
| <kbd>an</kbd> | `newsticker`查看RSS订阅      |

搜索相关的`Leader`键绑定:

| key           | function                                                               |
|---------------|------------------------------------------------------------------------|
| <kbd>si</kbd> | `imenu`                                                                |
| <kbd>sj</kbd> | `evil-show-jumps`                                                      |
| <kbd>sm</kbd> | `evil-show-marks`                                                      |
| <kbd>sr</kbd> | `evil-show-registers`                                                  |

与文本相关的`Leader`键绑定:

| key              | function                                     |
|------------------|----------------------------------------------|
| <kbd>x TAB</kbd> | `indent-rigidly`在一个`region`上统一进行缩进 |
| <kbd>xw</kbd>    | `delete-trailing-whitespace`删除行末空白字符 |
| <kbd>xj</kbd>    | `set-justification`文本对齐                  |

与代码相关的`Leader`键绑定:

| key           | function                                          |
|---------------|---------------------------------------------------|
| <kbd>cc</kbd> | `compile`编译                                     |
| <kbd>cC</kbd> | `recompile`重新编译                               |
| <kbd>ck</kbd> | `kill-compilation`打断当前的编译过程              |
| <kbd>cx</kbd> | `quickrun`快速运行当前程序                        |
| <kbd>cX</kbd> | `quickrun-shell`在`eshell`里查看输出              |
| <kbd>cd</kbd> | `rmsbolt-compile`查看编译器的输出，如汇编、IR表示 |

## Emacs

| key                | function                                                           |
|--------------------|--------------------------------------------------------------------|
| <kbd>M-;</kbd>     | `comment-or-uncomment` 注释与反注释                                |
| <kbd>C-c '</kbd>   | 通过`separedit`在注释中快乐地写代码                                |
| <kbd>C-c x</kbd>   | 调用`quickrun`来运行当前`buffer`内的代码。`eval`快人一步！         |
| <kbd>C-c p</kbd>   | `projectile`调用前缀，方便地在项目内跳转、编译等其他功能           |
| <kbd>C-x g</kbd>   | 呼出 `magit`                                                       |
| <kbd>C-M-i</kbd>   | 在`git-commit`时会有`flyspell`检查单词是否错误，通过此按键自动修正 |
| <kbd>C-x o</kbd>   | 开启`repeat-mode`, 方便原生`C-x o`切换`window`                     |
| <kbd>M-g M-l</kbd> | 调用`avy-goto-line`                                                |

因为[projectile](https://github.com/bbatsov/projectile)比较常用，把它单独拿出来
说。

| key                  | function                                                             |
|----------------------|----------------------------------------------------------------------|
| <kbd>C-c p f</kbd>   | `projectile-find-file`在项目内查找其他文件                           |
| <kbd>C-c p b</kbd>   | `projectile-switch-to-buffer`切换至其他`buffer`(限定在本`project`下) |
| <kbd>C-c p C</kbd>   | `projectile-configure-project`配置当前项目                           |
| <kbd>C-c p c</kbd>   | `projectile-compile-project`编译当前项目                             |
| <kbd>C-c p u</kbd>   | `projectile-run-project`运行当前项目                                 |
| <kbd>C-c p P</kbd>   | `projectile-test-project`测试当前项目                                |
| <kbd>C-c p p</kbd>   | `projectile-switch-project`切换至其他项目                            |
| <kbd>C-c p s r</kbd> | `projectile-ripgrep`使用`ripgrep`来搜索当前项目内的文本。            |

基于同样的理由，把`flycheck`单独拎了出来。

| key                | function                                  |
|--------------------|-------------------------------------------|
| <kbd>C-c ! l</kbd> | `flycheck-list-errors`列出所有`lint`错误  |
| <kbd>C-c ! n</kbd> | `flycheck-next-error`下一个`lint`错误     |
| <kbd>C-c ! p</kbd> | `flycheck-previous-error`上一下`lint`错误 |

更详细的按键绑定请直接看[代码](lisp/init-evil.el). :-)

<kbd>C-c h</kbd>是所有`hydra`的前缀，目前有 2 个，分别是:

1. `hydra-macro`方便执行`kmacro`
2. `hydra-other-window-scroll`在不改变焦点的情况下移动另一窗口的`buffer`

# 通用开发设置

- 显示行末空白字符
- 高亮**TODO** **FIXME**等关键字
- `dumb-jump`作为`lsp-find-definition`失败后的备份手段
- `magit`作为`git`客户端
- `hideshow`来显示/隐藏结构化的代码块，如 "{ }" 函数体等
- `rmsbolt`作为一个本地的 **Compiler Explorer** 相比于`godbolt`快速一点
- `ispell`拼写检查器, `evil`用户可以快速通过<kbd>z=</kbd> (`ispell-word`) 来检查
- `flyspell`拼写检查器，仅在`magit`写提交信息时启用
- `quickrun`作为一个能够执行部分区域内的代码块，方便快速验证函数功能
- `tempo`作为代码片段展开工具, `spdx`然后再<kbd>M-x tempo-expand-if-complete</kbd>即可

# prog-mode

## cc-mode

使用`lsp-mode`作为补全、符号查找的工具，默认后端使用`clangd`，一般发行版的源里都
会有对应的包。如果想使用[ccls][ccls]，可以`customize`对应的变量:

``` emacs-lisp
(setq lsp-clients-clangd-executable "ccls"
      lsp-clients-clangd-args nil)
```

如果想使用`ccls`的`lsp`扩展功能，需要安装[ccls][emacs-ccls]扩展。

此外，

- `cmake-mode`可使用`company-mode`进行符号补全
- 启用了`hide-ifdef-mode`，可以令`#if 0`到`#endif`之间的代码看起来像注释一样。也可以`#define`一些宏，放入`hide-ifdef-env`中生效。
- 部分常用`snippet`，如`ifndef`,`main`等等。详细列表见[`init-cpp.el`](lisp/lang/init-cpp.el)文件
- `cmake-mode`增加了一个简单 lib 的`snippet`，可以通过`lib`关键字展开

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

使用`lsp-mode`作为补全、符号查找的工具。在`Arch Linux`上，可以使用 [ocaml-lsp-git][ocaml-lsp-git] 这个包。

由于`ocaml-lsp-git`目前**只**实现了`lsp-format-buffer`且额外依赖`ocamlformat`。

所以这里额外使用了[ocp-indent][ocp-indent]，通过`ocp-indent-region`, `ocp-indent-buffer`来提供格式化代码的功能。

同时也集成了[dune][dune]。

`ocp-indent`和`dune`都依赖系统级别的包。

如果你是`Arch Linux`可以直接通过如下命令安装:

``` bash
yay -S ocaml-ocp-indent dune
```

## haskell-mode

非常纯粹, ~~其实是平常不怎么写 haskell~~。

# 截图

![dashboard](https://user-images.githubusercontent.com/4024656/83343041-46cbc480-a328-11ea-800d-6cd88540186e.png
"Dashboard")

![magit_markdown](https://user-images.githubusercontent.com/4024656/83343152-30723880-a329-11ea-90ff-ec5f3b7d504a.png
"Magit and markdown")

![cpp_company](https://user-images.githubusercontent.com/4024656/83343182-965ec000-a329-11ea-862c-a0b25305a1b4.png
"cc-mode and company")

# FAQ

1. [dashboard][dashboard] 里图标显示不正确？
   依赖 [all-the-icons][all-the-icons], 请确保`M-x all-the-icons-install-fonts`安
   装对应的字体以显示图标。
2. 更新时提示对应版本的包不存在？
   这多是因为国内镜像源同步慢导致的。如果出错，可以临时禁用镜像源。

``` elisp
(setq package-archives
      '(("melpa"  . "https://melpa.org/packages/")
        ("gnu"    . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
```
3. 如果升级包遇到了错误，可以使用 [init-mini.el](init-mini.el) 这个最小的配置来
   启动。

``` bash
emacs -Q -l init-mini.el
```

# 其他

欢迎提`issue`给出建议，感谢！

[all-the-icons]: https://github.com/domtronn/all-the-icons.el/
[ccls]: https://github.com/MaskRay/ccls/
[cargo]: https://melpa.org/#/cargo
[dashboard]: https://github.com/emacs-dashboard/emacs-dashboard/
[dune]: https://github.com/ocaml/dune/
[valign]: https://github.com/casouri/valign/
[emacs-ccls]: https://melpa.org/#/ccls
[ocp-indent]: https://melpa.org/#/ocp-indent
[ocaml-lsp-git]: https://aur.archlinux.org/packages/ocaml-lsp-git
[evil-snipe]: https://github.com/hlissner/evil-snipe
