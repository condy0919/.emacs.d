<h3 align="center">A fast and incredible Emacs config</h3>

<p align="center">
  <img src="https://upload.wikimedia.org/wikipedia/commons/thumb/0/08/EmacsIcon.svg/120px-EmacsIcon.svg.png" />
</p>

<div align="center">

[![Build Status](https://github.com/condy0919/.emacs.d/workflows/CI/badge.svg)](https://github.com/condy0919/.emacs.d/actions)
[![License](http://img.shields.io/:license-gpl3-blue.svg)](LICENSE)
![Supports Emacs 31+](https://img.shields.io/badge/Supports-Emacs_31%2B-blueviolet.svg?style=flat-square&logo=GNU%20Emacs&logoColor=white)

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
    - [其他语言](#其他语言)
- [截图](#截图)
- [FAQ](#faq)
- [其他](#其他)

<!-- markdown-toc end -->

个人**Emacs**配置
====

![overview](https://user-images.githubusercontent.com/4024656/83358470-63a6dd00-a3a6-11ea-8489-f86b08b48d13.png
"Emacs Overview")

仿 [Centaur Emacs](https://github.com/seagle0128/.emacs.d) 的个人配置，面向 **Emacs 31+**。

```bash
git clone --depth 1 https://github.com/condy0919/.emacs.d ~/.emacs.d
```

主力配置围绕 **C/C++/Rust/OCaml/Haskell** 展开，顺带覆盖 Python、Emacs Lisp、Shell、Bazel、CMake 等；日常写作与工具则包含 Org、Markdown、Gnus、rcirc 和 `gptel`。

保持着尽量使用 `Emacs` 自带功能的原则，能用自带的就用自带的。

# 需要的依赖

- `hunspell` (optional) 拼写检查，目前仅在 `git-commit-mode` 下默认启用
- `rg` 更快的 `grep`
- `cmake` `c++` 项目的构建工具
- `clangd` / `ccls` / `rust-analyzer` (optional) 对应语言的 LSP 后端
- `git` 这个就不用说了吧？
- `gcc` 这个就不用说了吧？
- `terminal-notifier` (optional) macOS 下发送任务通知

# 基础配置

最基础的配置包含了那些在所有 `mode` 下都不会变更的设置，主要用到这些自带功能：

| 包名          | 功能                                             |
|---------------|--------------------------------------------------|
| align         | `align-regexp` 可以自动对齐选择的符号            |
| appt          | 任务提醒，可以与 `org-mode` 结合                 |
| disp-table    | `prettify-special-glyphs-mode` 美化折行/截断符号 |
| editorconfig  | 读取项目 `.editorconfig` 缩进风格                |
| hippie-expand | 用来展开文本                                     |
| hl-line       | 高亮当前行（GUI 下启用）                         |
| newcomment    | 注释、反注释功能                                 |
| paren         | 高亮匹配的括号                                   |
| repeat        | 减少连续按键的前缀重复                           |
| saveplace     | 自动记录上次打开文件的位置                       |
| simple        | 在 `modeline` 里显示行号、列号以及当前文本的大小 |
| so-long       | 打开长行的文件不再痛苦 (`Emacs` 27+ 自带)        |
| tab-bar       | 窗口布局管理 (`Emacs` 27+ 自带)                  |
| tramp         | 远程编辑就靠它                                   |
| tty-tip       | 终端下使用 childframe 显示 tooltip               |
| vc            | `vc-auto-revert-mode` 自动刷新受控文件           |
| which-key     | 按键前缀提示 (`Emacs` 30+ 自带)                  |

为了保持界面的整洁，在 `early-init.el` 里提前禁用了工具栏、滚动条和非 macOS 下的菜单栏。默认使用空格缩进，`tab-width` 为 4，`fill-column` 为 80，同时关闭了备份文件、自动保存和锁文件。

# 插件配置、升级

`package.el` (自带的) 来安装包、`use-package` (同样自带) 来管理配置。Git 仓库包（例如 [lsp-hierarchy][lsp-hierarchy]）直接用内置的 `:vc` 关键字拉取。

至于 ELPA/MELPA 里没有的单文件包（比如 `llvm-mode.el` 和 `tablegen-mode.el`），以前先后折腾过 `straight.el` 和 `quelpa`，现在直接用 Emacs 31 原生的 `user-lisp/` 目录就行了。启动时 Emacs 会自动对 `user-lisp/` 下的文件做字节编译、扫描 autoload 并加入 `load-path`。不过这两个 LLVM 相关文件不会自动下载，需要按 [`init-cpp.el`](lisp/lang/init-cpp.el) 注释里的 `curl` 命令手动放进 `~/.emacs.d/user-lisp/`。

Emacs 29 引入了 `package-upgrade-all`，需要更新直接 <kbd>M-x package-upgrade-all</kbd> 即可（或者直接点 Dashboard 上的 **Upgrade**）。

CI 使用 Emacs 31.1 检查启动，并额外运行允许失败的 snapshot 作兼容性参考。在仓库根目录可以运行相同的检查：

```bash
# 验证启动检查能识别错误；使用临时配置和独立进程，不下载包
emacs -Q --batch -l .github/check-init-tests.el -f ert-run-tests-batch-and-exit

# 检查当前配置的启动路径；会安装缺失的依赖包
emacs -Q --batch -l .github/check-init.el -f ci-check-init
```

启动检查依次加载 `early-init.el`、激活包、处理 `user-lisp/`、加载 `init.el`，然后执行 `after-init-hook` 和 `emacs-startup-hook`。未捕获的错误以及错误级告警都会使检查失败，普通告警仍保留在日志中。该检查不覆盖 GUI frame 行为或尚未触发的语言工具。包缓存按 Emacs 构建版本和配置内容区分，缓存命中也仍会检查、安装缺失依赖。

# 界面

GUI 使用 `doom-one`，简直惊艳！`doom-one` 的界面非常好看！终端则切换到 `doom-Iosvkem`，状态栏用 `doom-modeline`，弹出窗口交给 `shackle` 管。

# 趁手的工具

`rg` 是比较常用的工具，更有 `projectile` 管理项目，让项目编译、测试、运行变得更加方便。

`avy` 用来代替 `vim-easymotion`。而且 `avy` 还提供了 `goto-line` 的功能，这下都不用开相对行号 `8k` `9j` 这样跳了。

以前是 `ivy` 用户，现在则是仅使用 `vertico`、`embark` 和 `consult` 了（候选详细信息直接交给内置的 `completions-detailed`）。另外 `consult` 重映射了不少内置命令，比如 `imenu` → `consult-imenu`、`bookmark-jump` → `consult-bookmark`、`recentf-open-files` → `consult-recent-file`。

`Emacs` 下的 `org-mode`/`markdown-mode` 让人惊艳，突然觉得写文档也会这么快乐。与之相辅相成的还有 `separedit`，让人在代码里写 `documentation comments` 不再烦恼。

[valign][valign] 提供了像素级别的表格对齐，终于不用再靠西文半宽的字体了！

AI 辅助用的是 `gptel`（配了 DeepSeek 后端，凭据走 `auth-source`），写 commit message 时加载 Magit 后可以直接调用 `M-x gptel-commit`。

从 `neovim` 迁移过来的我，自然是常开 `evil-mode`，相关的 `evil` 套件有：

- `evil` (使用内置 `undo-redo`，`:q` 只关当前 buffer)
- `evil-collection`
- `evil-surround`

# 按键绑定

## evil-mode

开启了 `evil-collection` 的 Unimpaired 绑定及 `repeat-mode` 集成，获得如下键绑定：

| key              | function                                                              |
|------------------|-----------------------------------------------------------------------|
| <kbd>[b</kbd>    | `evil-prev-buffer` 切换至上一个 `buffer`                              |
| <kbd>]b</kbd>    | `evil-next-buffer` 切换至下一个 `buffer`                              |
| <kbd>[e</kbd>    | `evil-collection-unimpaired-move-text-up` 将当前行移动至上一行        |
| <kbd>]e</kbd>    | `evil-collection-unimpaired-move-text-down` 将当前行移动至下一行      |
| <kbd>[l</kbd>    | `evil-collection-unimpaired-previous-error` 上一个错误                |
| <kbd>]l</kbd>    | `evil-collection-unimpaired-next-error` 下一个错误                    |
| <kbd>[ SPC</kbd> | `evil-collection-unimpaired-insert-newline-above` 在上方插入一空行    |
| <kbd>] SPC</kbd> | `evil-collection-unimpaired-insert-newline-below` 在下方插入一空行    |
| <kbd>[u</kbd>    | `evil-collection-unimpaired-url-encode` 对所选内容进行 `url` 参数编码 |
| <kbd>]u</kbd>    | `evil-collection-unimpaired-url-decode` 对所选内容进行 `url` 参数解码 |

此外，凭借 `avy` 模拟了 [evil-snipe][evil-snipe] 的 `s` 和 `f` 功能。

| key          | function                     |
|--------------|------------------------------|
| <kbd>s</kbd> | `evil-avy-goto-char-timer`   |
| <kbd>f</kbd> | `evil-avy-goto-char-in-line` |

本配置里使用 `hideshow` 来 `fold` 代码块。由于 `hideshow` 本身提供的快捷键非常长，非常推荐使用 `evil-mode` 在 `normal` 状态下定义的键绑定。

| key           | function                                      |
|---------------|-----------------------------------------------|
| <kbd>zm</kbd> | `evil-close-folds` 隐藏所有代码块             |
| <kbd>zr</kbd> | `evil-open-folds` 显示所有被隐藏的代码块      |
| <kbd>zo</kbd> | `evil-open-fold` 展开当前代码块               |
| <kbd>zO</kbd> | `evil-open-fold-rec` 递归展开当前及内部代码块 |
| <kbd>zc</kbd> | `evil-close-fold` 收起当前代码块              |
| <kbd>za</kbd> | `evil-toggle-fold` 来切换是否隐藏代码         |

`Leader` 为普通模式下的 `SPC`，`Localleader` 为 `SPC m`。

全局/通用 `Leader` 键绑定如下：

| key              | function                                              |
|------------------|-------------------------------------------------------|
| <kbd>SPC</kbd>   | `keyboard-escape-quit` 退出 minibuffer / 取消当前操作 |
| <kbd>S-SPC</kbd> | `lazy-highlight-cleanup` 清除搜索高亮                 |
| <kbd>'</kbd>     | `vertico-repeat` 恢复上一次 Vertico 补全              |
| <kbd>;</kbd>     | `avy-resume` 恢复上一次 avy 跳转                      |

与文件相关的 `Leader` 键绑定如下：

| key           | function                                             |
|---------------|------------------------------------------------------|
| <kbd>ff</kbd> | `find-file` 打开文件                                 |
| <kbd>fF</kbd> | `find-file-other-window` 同上，不过是在另一窗口打开  |
| <kbd>f/</kbd> | 同上                                                 |
| <kbd>fD</kbd> | `+delete-current-file` 删除当前文件                  |
| <kbd>fC</kbd> | `+copy-current-file` 拷贝当前文件至其他地方          |
| <kbd>fy</kbd> | `+copy-current-filename` 拷贝当前文件的绝对路径      |
| <kbd>fR</kbd> | `rename-visited-file` 重命名当前文件                 |
| <kbd>fr</kbd> | `recentf-open-files` 访问最近使用过的文件            |
| <kbd>fl</kbd> | `find-file-literally` 采用朴素模式打开文件           |
| <kbd>fo</kbd> | `find-sibling-file` 寻找并打开关联的兄弟文件         |
| <kbd>fj</kbd> | `dired-jump` 进入当前文件的目录                      |
| <kbd>fJ</kbd> | `dired-jump-other-window` 同上，不过是在另一窗口打开 |

与 `buffer`、`bookmark` 相关的键绑定：

| key           | function                                                                                   |
|---------------|--------------------------------------------------------------------------------------------|
| <kbd>bb</kbd> | `switch-to-buffer` 切换 `buffer`                                                           |
| <kbd>bB</kbd> | `switch-to-buffer-other-window` 同上，不过是在另一窗口打开                                 |
| <kbd>bc</kbd> | `clone-indirect-buffer` 将当前 `buffer` 克隆至另一 `buffer`，它们可以使用不同 `major-mode` |
| <kbd>bC</kbd> | `clone-indirect-buffer-other-window` 同上，不过是在另一窗口打开                            |
| <kbd>by</kbd> | `+copy-current-buffer-name` 复制当前 `buffer` 的名字                                       |
| <kbd>bv</kbd> | `revert-buffer-quick` 快速重新读取当前 `buffer` 对应的文件                                 |
| <kbd>bx</kbd> | `scratch-buffer` 直接跳转到 `*scratch*` buffer                                             |
| <kbd>bz</kbd> | `bury-buffer` 退出当前 `buffer` 的显示，当前 `buffer` 未被 kill                            |

| key           | function                                                 |
|---------------|----------------------------------------------------------|
| <kbd>bj</kbd> | `bookmark-jump` 跳转至书签                               |
| <kbd>bJ</kbd> | `bookmark-jump-other-window` 同上，不过是在另一窗口打开  |
| <kbd>bm</kbd> | `bookmark-set` 设置书签                                  |
| <kbd>bM</kbd> | `bookmark-set-no-overwrite` 同上，但是不会覆盖同名的书签 |
| <kbd>bd</kbd> | `bookmark-delete` 删除书签                               |
| <kbd>bi</kbd> | `bookmark-insert` 插入书签的内容                         |
| <kbd>bl</kbd> | `bookmark-bmenu-list` 打开书签列表                       |
| <kbd>br</kbd> | `bookmark-rename` 重命名书签                             |
| <kbd>bs</kbd> | `bookmark-save` 保存书签                                 |
| <kbd>bw</kbd> | `bookmark-write` 将书签保存至其他文件                    |

与窗口相关的 `Leader` 键绑定如下：

| key           | function                                                          |
|---------------|-------------------------------------------------------------------|
| <kbd>w</kbd>  | `evil-window-map` 窗口操作的前缀键 (等同于 `C-w`)                 |
| <kbd>wx</kbd> | `kill-buffer-and-window` 关闭当前 buffer 并关闭窗口               |
| <kbd>wu</kbd> | `tab-bar-history-back` 回退窗口布局（`repeat-mode` 下 `u`/`r`）   |
| <kbd>w-</kbd> | `split-window-vertically` 水平分割窗口                            |
| <kbd>w/</kbd> | `split-window-horizontally` 垂直分割窗口                          |

与标签页 (Tab) 相关的 `Leader` 键绑定如下：

| key           | function                                              |
|---------------|-------------------------------------------------------|
| <kbd>tc</kbd> | `tab-bar-close-tab` 关闭当前标签页                    |
| <kbd>tC</kbd> | `tab-bar-close-group-tabs` 关闭当前标签组的所有标签页 |
| <kbd>tg</kbd> | `tab-bar-change-tab-group` 修改当前标签页的分组       |
| <kbd>ti</kbd> | `tab-switcher` 切换标签页                             |
| <kbd>tn</kbd> | `tab-bar-new-tab` 新建标签页                          |
| <kbd>to</kbd> | `tab-bar-close-other-tabs` 关闭其他标签页             |
| <kbd>tt</kbd> | `tab-bar-switch-to-tab` 切换到指定的标签页            |
| <kbd>t'</kbd> | `tab-bar-switch-to-recent-tab` 切换到最近访问的标签页 |
| <kbd>tr</kbd> | `tab-bar-rename-tab` 重命名当前标签页                 |

与项目相关的 `Leader` 键绑定如下：

| key          | function                                                            |
|--------------|---------------------------------------------------------------------|
| <kbd>p</kbd> | `projectile-command-map` 项目管理前缀键 (功能与下文的 `C-c p` 相同) |

打开其他程序的 `Leader` 键绑定：

| key           | function                              |
|---------------|---------------------------------------|
| <kbd>ot</kbd> | `ansi-term` 打开 `ansi-term`          |
| <kbd>oe</kbd> | `eshell` 打开 `eshell`                |
| <kbd>os</kbd> | `shell` 打开 `shell`                  |
| <kbd>ol</kbd> | `org-store-link` 存储 URL             |
| <kbd>oc</kbd> | `org-capture` 随时记录一些想法、URL等 |

打开一些看起来像是独立的应用：

| key           | function          |
|---------------|-------------------|
| <kbd>aa</kbd> | `org-agenda` 日程 |
| <kbd>ac</kbd> | `calendar` 日历   |
| <kbd>ag</kbd> | `gnus` 查看新闻组 |
| <kbd>ai</kbd> | `rcirc` 上 IRC    |

搜索相关的 `Leader` 键绑定：

| key           | function              |
|---------------|-----------------------|
| <kbd>si</kbd> | `imenu`               |
| <kbd>sj</kbd> | `evil-show-jumps`     |
| <kbd>sm</kbd> | `evil-show-marks`     |
| <kbd>sr</kbd> | `evil-show-registers` |
| <kbd>sp</kbd> | `consult-ripgrep`     |
| <kbd>ss</kbd> | `consult-line`        |

与代码相关的 `Leader` 键绑定：

| key           | function                                                   |
|---------------|------------------------------------------------------------|
| <kbd>cd</kbd> | `rmsbolt-compile` 查看编译器的输出，如汇编、IR表示         |
| <kbd>cc</kbd> | `compile` 编译                                             |
| <kbd>cC</kbd> | `recompile` 重新编译                                       |
| <kbd>ck</kbd> | `kill-compilation` 打断当前的编译过程                      |
| <kbd>cl</kbd> | `+switch-to-compilation` 快速切换到 `*compilation*` buffer |
| <kbd>cw</kbd> | `delete-trailing-whitespace` 删除行末空白字符              |
| <kbd>cx</kbd> | `quickrun` 快速运行当前程序                                |

## Emacs

| key                | function                                    |
|--------------------|---------------------------------------------|
| <kbd>M-\`</kbd>    | `eshell-toggle` 打开一个弹出式 `eshell`     |
| <kbd>M-;</kbd>     | `comment-or-uncomment` 注释与反注释         |
| <kbd>C-c '</kbd>   | 通过 `separedit` 在注释中快乐地写代码       |
| <kbd>C-c p</kbd>   | `projectile` 调用前缀                       |
| <kbd>C-x g</kbd>   | 呼出 `magit`                                |
| <kbd>M-g M-l</kbd> | 调用 `avy-goto-line`                        |

因为 [projectile][projectile] 比较常用，故把它单独拿出来说。

| key                  | function                                                                 |
|----------------------|--------------------------------------------------------------------------|
| <kbd>C-c p f</kbd>   | `projectile-find-file` 在项目内查找其他文件                              |
| <kbd>C-c p b</kbd>   | `projectile-switch-to-buffer` 切换至其他 `buffer`(限定在本 `project` 下) |
| <kbd>C-c p c o</kbd> | `projectile-configure-project` 配置当前项目                              |
| <kbd>C-c p c c</kbd> | `projectile-compile-project` 编译当前项目                                |
| <kbd>C-c p c r</kbd> | `projectile-run-project` 运行当前项目                                    |
| <kbd>C-c p c t</kbd> | `projectile-test-project` 测试当前项目                                   |
| <kbd>C-c p p</kbd>   | `projectile-switch-project` 切换至其他项目                               |
| <kbd>C-c p s r</kbd> | `projectile-ripgrep` 使用 `ripgrep` 来搜索当前项目内的文本。             |

基于同样的理由，把 `flycheck` 单独拎了出来。

| key                | function                                     |
|--------------------|----------------------------------------------|
| <kbd>C-c ! l</kbd> | `flycheck-list-errors` 列出所有 `lint` 错误  |
| <kbd>C-c ! n</kbd> | `flycheck-next-error` 下一个 `lint` 错误     |
| <kbd>C-c ! p</kbd> | `flycheck-previous-error` 上一个 `lint` 错误 |

更详细的按键绑定请直接看[代码](lisp/init-evil.el). :-)

<kbd>C-c h</kbd> 是两个 `transient` 菜单的前缀：

1. `C-c h t` (`background-opacity-menu`) 调整背景透明度
2. `C-c h o` (`scroll-other-window-menu`) 在不改变焦点的情况下滚动另一窗口的 `buffer`

# 通用开发设置

- `prog-mode` 下默认启用 `company`、`flycheck` 与 `lsp-deferred`（`C-c l` 前缀，`C-c l g h` 查看调用层级）
- 显示行末空白字符，启用 `editorconfig-mode` 读取项目编码风格
- 高亮 **TODO**、**FIXME** 等关键字
- `citre` 作为 ctags 跳转入口（`C-c c j`），`rg` 和 `devdocs` (`C-c b`) 辅助查代码与文档
- `magit` 作为 `git` 客户端，配合 `diff-hl` 显示变更标记及 `vc-auto-revert-mode` 自动刷新
- `hideshow` 来显示/隐藏结构化的代码块，如 "{ }" 函数体等
- `rmsbolt` 作为一个本地的 **Compiler Explorer**，相比于 `godbolt` 快速一点
- `ispell` 拼写检查器，`evil` 用户可以快速通过 <kbd>z=</kbd> (`ispell-word`) 来检查
- `flyspell` 拼写检查器，仅在 `magit` 写提交信息时启用
- `quickrun` 能够快速运行当前文件/区域，方便验证代码片段
- `tempo` 作为代码片段展开工具，`spdx` 提供许可证头，配合 <kbd>M-x tempo-expand-if-complete</kbd> 或 `hippie-expand` 触发

# prog-mode

## cc-mode

使用 `lsp-mode` 作为补全、符号查找的工具，默认后端优先选择 `clangd`（启用了后台索引和 clang-tidy），找不到时回退到 `ccls`。

内置 `find-sibling-rules` 来在头文件、源文件、`_test.cpp` 与 `_benchmark.cpp` 之间跳转（`SPC f o`）。

此外，

- `cmake-mode` 可使用 `company-cmake` 进行符号补全，配合 `cmake-font-lock` 高亮，`C-c d` 查看 CMake 帮助
- 部分常用 `snippet`，如 `ifndef`、`externC`、`main`、`mv`、`cp`、`ns` 等，详细列表见 [`init-cpp.el`](lisp/lang/init-cpp.el)
- `cmake-mode` 内置了 `lib`、`umbrella`、`comp`、`test`、`bench`、`presets` 等现代 CMake 模板

## rust-mode

使用 `lsp-mode` + `rust-analyzer` 作为补全、符号查找的工具，调好了 `where` 子句缩进并加载 `rust-load-optional-libraries` 自带的辅助库（配置里禁用了 `unresolved-extern-crate` 诊断）。

## ocaml-mode

使用 `tuareg-mode`，进入时启用 `merlin`（`ocamlmerlin`）；如果系统装了 `ocp-indent` 和 `dune` 也会一并启用。

`merlin`、`ocp-indent` 和 `dune` 的 elisp 文件均使用 `:ensure nil` 依赖系统包，如果你是 `Arch Linux` 可以直接通过如下命令安装：

``` bash
yay -S ocaml-ocp-indent dune
```

## haskell-mode

非常纯粹，~~其实是平常不怎么写 haskell~~。

## 其他语言

- `python-mode` 按项目隔离交互式解释器，Python 与 C/C++/Rust 均开启 `superword-mode`
- Emacs Lisp 直接用内置支持，常用求值快捷键绑在 `SPC m` 下（如 `SPC m e b`、`SPC m e d`、`SPC m e e`）
- 另有 Shell、Bazel、YAML、Graphviz、Systemd 和 Bison/Flex 配置，详见 [init-dev.el](lisp/init-dev.el) 和 [lisp/lang/](lisp/lang/)

# 截图

![dashboard](https://user-images.githubusercontent.com/4024656/83343041-46cbc480-a328-11ea-800d-6cd88540186e.png
"Dashboard")

![magit_markdown](https://user-images.githubusercontent.com/4024656/83343152-30723880-a329-11ea-90ff-ec5f3b7d504a.png
"Magit and markdown")

![cpp_company](https://user-images.githubusercontent.com/4024656/83343182-965ec000-a329-11ea-862c-a0b25305a1b4.png
"cc-mode and company")

# FAQ

## [dashboard][dashboard] 图标显示异常

依赖 [nerd-icons][nerd-icons]，请确保 `M-x nerd-icons-install-fonts` 安装对应的字体以显示图标。

## 更新时提示对应包版本不存在

如果您在使用国内镜像源时出现这个问题，多数情况都是由镜像源同步不一致导致的，可以切换成上游来规避这个问题。

``` elisp
(setq package-archives
      '(("melpa"  . "https://melpa.org/packages/")
        ("gnu"    . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
```

## Emacs 配置挂了

先用 `emacs --debug-init` 看看报错，也可以使用 [init-mini.el](init-mini.el) 这个最小配置来临时救急一下。

``` bash
emacs -Q -l ~/.emacs.d/init-mini.el
```

~~虽然咱都是直接开 nvim 的~~

# 其他

欢迎提 `issue` 给出建议，感谢！

[nerd-icons]: https://github.com/rainstormstudio/nerd-icons.el
[dashboard]: https://github.com/emacs-dashboard/emacs-dashboard/
[valign]: https://github.com/casouri/valign/
[evil-snipe]: https://github.com/hlissner/evil-snipe
[projectile]: https://github.com/bbatsov/projectile
[lsp-hierarchy]: https://github.com/condy0919/lsp-hierarchy
