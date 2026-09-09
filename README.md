<h3 align="center">Emacs config for coding and writing</h3>

<p align="center">
  <img src="https://upload.wikimedia.org/wikipedia/commons/thumb/0/08/EmacsIcon.svg/120px-EmacsIcon.svg.png" />
</p>

<div align="center">

[![Build Status](https://github.com/condy0919/.emacs.d/workflows/CI/badge.svg)](https://github.com/condy0919/.emacs.d/actions)
[![License](http://img.shields.io/:license-gpl3-blue.svg)](LICENSE)
![Supports Emacs 31+](https://img.shields.io/badge/Supports-Emacs_31%2B-blueviolet.svg?style=flat-square&logo=GNU%20Emacs&logoColor=white)

</div>

# 个人 Emacs 配置

面向 **Emacs 31 及以上版本**的个人配置，参考了 [Centaur Emacs](https://github.com/seagle0128/.emacs.d)。以 Evil 提供 Vim 风格编辑，使用 Company、LSP、Projectile 和 Consult 完成日常开发，优先采用 Emacs 内置功能。

除了 C/C++、Rust、OCaml 和 Haskell，还包含 Python、Emacs Lisp、Shell、Bazel、CMake 等配置，以及 Org、Markdown、邮件、IRC 和 AI 辅助工具。

## 目录

- [安装与依赖](#安装与依赖)
- [配置结构](#配置结构)
- [基础配置与界面](#基础配置与界面)
- [插件安装与升级](#插件安装与升级)
- [补全与搜索](#补全与搜索)
- [按键绑定](#按键绑定)
- [通用开发设置](#通用开发设置)
- [语言支持](#语言支持)
- [写作与日常工具](#写作与日常工具)
- [截图](#截图)
- [FAQ](#faq)

# 安装与依赖

已有配置时，请先备份 `~/.emacs.d`。在该目录不存在时执行：

```bash
git clone --depth 1 https://github.com/condy0919/.emacs.d ~/.emacs.d
```

首次启动需要联网安装声明了 `:ensure t` 的包。语言服务器、编译器和部分系统提供的 Lisp 包需要另行准备。

| 依赖                       | 用途                                         |
|----------------------------|----------------------------------------------|
| Emacs 31+、Git             | 运行配置、管理仓库和使用 Magit               |
| `rg`                       | Consult、rg.el 等工具使用的 ripgrep 搜索程序 |
| C/C++ 编译器、CMake        | 按项目需要安装，用于编译、运行和 CMake 补全  |
| `clangd` 或 `ccls`         | C/C++ 语言服务器；配置优先选择 `clangd`      |
| `rust-analyzer`            | 使用 Rust LSP 时需要                         |
| `hunspell` 及 `en_US` 字典 | 拼写检查；Git 提交编辑时默认启用 Flyspell    |
| `terminal-notifier`        | macOS 下的任务通知                           |

图标字体可通过 `M-x nerd-icons-install-fonts` 安装。使用其他语言的 LSP、调试器或 Quickrun 时，也需要对应工具链。

这是一份个人配置，使用前还应检查以下设置：

- Org 默认目录为 `~/.org/`，任务文件为 `tasks.org`。
- OCaml 的 `merlin`、`ocp-indent`、`dune` 使用 `:ensure nil`，不会由本配置自动安装。
- 邮件、IRC 和 gptel 的服务器、身份与凭据需要按自己的环境设置。

# 配置结构

| 文件或目录                                                    | 用途                                       |
|---------------------------------------------------------------|--------------------------------------------|
| `early-init.el`                                               | 启动早期的垃圾回收和窗口设置               |
| `init.el`                                                     | 包管理、加载模块，最后加载 `custom.el`     |
| `init-mini.el`                                                | 调试用的精简入口，加载基础配置和 Icomplete |
| `lisp/init-base.el`                                           | 编辑、补全、历史、日历和通知等基础设置     |
| `lisp/init-evil.el`                                           | Evil、Leader 和 Localleader 按键           |
| `lisp/init-lsp.el`、`lisp/init-dev.el`                        | 补全、LSP、项目管理与开发工具              |
| `lisp/lang/`                                                  | 各语言的模式、缩进和代码片段               |
| `lisp/init-org.el`、`lisp/init-text.el`                       | Org 与文本编辑                             |
| `lisp/init-mail.el`、`lisp/init-shell.el`、`lisp/init-gpt.el` | 邮件、终端与 AI 辅助                       |
| `etc/`、`var/`                                                | no-littering 管理的配置文件和运行数据      |

`custom.el` 用于保存 Customize 设置，在主要模块之后加载。与包初始化时机有关的调整仍应放在对应模块中。

# 基础配置与界面

一些内置功能.

| 包名          | 功能                                           |
|---------------|------------------------------------------------|
| align         | `align-regexp`可以自动对齐选择的符号           |
| appt          | 任务提醒，可以与`org-mode`结合                 |
| hippie-expand | 用来展开文本                                   |
| hl-line       | 高亮当前行                                     |
| newcomment    | 注释、反注释功能                               |
| paren         | 高亮匹配的括号                                 |
| saveplace     | 自动记录上次打开文件的位置                     |
| simple        | 在`modeline`里显示行号、列号以及当前文本的大小 |
| so-long       | 打开长行的文件不再痛苦 (`Emacs` 27+ 自带)      |
| tab-bar       | 窗口布局管理 (`Emacs` 27+ 自带)                |
| tramp         | 远程编辑就靠它                                 |

默认使用空格缩进，`tab-width` 为 4，`fill-column` 为 80；具体语言模式可以进一步覆盖。配置关闭了备份文件、自动保存和锁文件。

GUI 使用 `doom-one`，简直惊艳！ `doom-one` 的界面非常好看！ 终端使用 `doom-Iosvkem`，状态栏使用 `doom-modeline`。Dashboard 显示最近文件、项目和包升级入口，Shackle 管理帮助、搜索、编译等弹出窗口。工具栏和滚动条默认隐藏，菜单栏的设置见 `early-init.el` 与 `init-base.el`。

# 插件安装与升级

使用内置 `package.el` 安装包，内置 `use-package` 组织配置，包源为 GNU ELPA、NonGNU ELPA 和 MELPA。`which-key` 使用内置版本，为按键前缀显示提示。

保留 Quelpa 用于单文件包。`llvm-mode` 和 `tablegen-mode` 不会自动下载，需要手动执行 [init-cpp.el](lisp/lang/init-cpp.el) 中的 Quelpa 安装代码，再打开相应文件。

升级包可运行 `M-x package-upgrade-all`，也可点击 Dashboard 的 **Upgrade**。普通包升级不会代替手工维护的本地包或系统工具升级。

# 补全与搜索

- **Vertico** 显示 minibuffer 候选，**Marginalia** 提供候选注释。
- **Consult** 提供全文搜索、行搜索、最近文件、书签和寄存器等命令。
- **Embark** 对候选执行操作：minibuffer 中按 `M-o` 调用操作菜单，`C-c C-c` 导出候选，`C-c C-o` 收集候选。
- **Company** 在编程模式下提供补全，整合 CAPF、Tempo、文件名和文本候选。
- **rg.el** 提供搜索界面，**wgrep** 用于编辑搜索结果。
- **Avy** 提供字符和行跳转，`valign` 对齐 Org、Markdown 表格。

Consult 重映射了若干内置命令，例如 `imenu` → `consult-imenu`、`bookmark-jump` → `consult-bookmark`、`recentf-open-files` → `consult-recent-file`。下方按键表保留配置中的原始绑定名称，实际调用会受到这些重映射影响；Evil 的 marks 和 jumps 也使用 evil-collection 的 Consult 界面。

# 按键绑定

## evil-mode

**Leader 为普通模式下的 `SPC`，Localleader 为 `SPC m`。** 下方 Leader 表省略前缀，例如 `ff` 表示依次按 `SPC f f`，通用表中的 `SPC` 表示 `SPC SPC`。

Evil 使用内置 `undo-redo`；`:q` 被重映射为关闭当前 buffer。文本开启 `visual-line-mode` 时，`j` / `k` 按视觉行移动。

使用 `evil-collection` 的 Unimpaired 绑定，并启用其 `repeat-mode` 集成：

| 按键 | 命令与说明 |
| ------------------ | --------------------------------------------------------------------- |
| <kbd>[b</kbd> | `evil-prev-buffer` 切换至上一个 `buffer` |
| <kbd>]b</kbd> | `evil-next-buffer` 切换至下一个 `buffer` |
| <kbd>[e</kbd> | `evil-collection-unimpaired-move-text-up` 将当前行移动至上一行 |
| <kbd>]e</kbd> | `evil-collection-unimpaired-move-text-down` 将当前行移动至下一行 |
| <kbd>[l</kbd> | `evil-collection-unimpaired-previous-error` 上一个错误 |
| <kbd>]l</kbd> | `evil-collection-unimpaired-next-error` 下一个错误 |
| <kbd>[ SPC</kbd> | `evil-collection-unimpaired-insert-newline-above` 在上方插入一空行 |
| <kbd>] SPC</kbd> | `evil-collection-unimpaired-insert-newline-below` 在下方插入一空行 |
| <kbd>[u</kbd> | `evil-collection-unimpaired-url-encode` 对所选内容进行 `url` 参数编码 |
| <kbd>]u</kbd> | `evil-collection-unimpaired-url-decode` 对所选内容进行 `url` 参数解码 |

此外，凭借 avy 模拟了 [evil-snipe][evil-snipe] 的 `s` 和 `f` 功能。

| 按键 | 命令与说明 |
| -------------- | ------------------------------ |
| <kbd>s</kbd> | `evil-avy-goto-char-timer` |
| <kbd>f</kbd> | `evil-avy-goto-char-in-line` |

使用 Hideshow 折叠代码，常用 Evil 普通模式按键如下：

| 按键 | 命令与说明 |
| --------------- | ----------------------------------------------------- |
| <kbd>zm</kbd> | `evil-close-folds` 隐藏所有代码块 |
| <kbd>zr</kbd> | `evil-open-folds` 显示所有被隐藏的代码块 |
| <kbd>zo</kbd> | `evil-open-fold` 展开当前代码块 |
| <kbd>zO</kbd> | `evil-open-fold-rec` 递归展开当前及内部代码块 |
| <kbd>zc</kbd> | `evil-close-fold` 收起当前代码块 |
| <kbd>za</kbd> | `evil-toggle-fold` 来切换是否隐藏代码 |

全局/通用 `Leader` 键绑定如下:

| 按键 | 命令与说明 |
| ------------------ | ------------------------------------------------------ |
| <kbd>SPC</kbd> | `keyboard-escape-quit` 退出 minibuffer / 取消当前操作 |
| <kbd>S-SPC</kbd> | `lazy-highlight-cleanup` 清除搜索高亮 |
| <kbd>'</kbd> | `vertico-repeat` 恢复上一次 Vertico 补全 |
| <kbd>;</kbd> | `avy-resume` 恢复上一次 avy 跳转 |

与文件相关的 `Leader` 键绑定如下:

| 按键 | 命令与说明 |
| --------------- | ----------------------------------------------------- |
| <kbd>ff</kbd> | `find-file` 打开文件 |
| <kbd>fF</kbd> | `find-file-other-window` 同上，不过是在另一窗口打开 |
| <kbd>f/</kbd> | 同上 |
| <kbd>fD</kbd> | `+delete-current-file` 删除当前文件 |
| <kbd>fC</kbd> | `+copy-current-file` 拷贝当前文件至其他地方 |
| <kbd>fy</kbd> | `+copy-current-filename` 拷贝当前文件的绝对路径 |
| <kbd>fR</kbd> | `rename-visited-file` 重命名当前文件 |
| <kbd>fr</kbd> | `recentf-open-files` 访问最近使用过的文件 |
| <kbd>fl</kbd> | `find-file-literally` 采用朴素模式打开文件 |
| <kbd>fo</kbd> | `find-sibling-file` 寻找并打开关联的兄弟文件 |
| <kbd>fj</kbd> | `dired-jump` 进入当前文件的目录 |
| <kbd>fJ</kbd> | `dired-jump-other-window` 同上，不过是在另一窗口打开 |

与 `buffer`、`bookmark` 相关的键绑定:

| 按键 | 命令与说明 |
| --------------- | --------------------------------------------------------------------------------------- |
| <kbd>bb</kbd> | `switch-to-buffer` 切换 `buffer` |
| <kbd>bB</kbd> | `switch-to-buffer-other-window` 同上，不过是在另一窗口打开 |
| <kbd>bc</kbd> | `clone-indirect-buffer` 将当前 `buffer` 克隆至另一 `buffer`，它们可以使用不同 `major-mode` |
| <kbd>bC</kbd> | `clone-indirect-buffer-other-window` 同上，不过是在另一窗口打开 |
| <kbd>by</kbd> | `+copy-current-buffer-name` 复制当前 `buffer` 的名字 |
| <kbd>bv</kbd> | `revert-buffer-quick` 快速重新读取当前 `buffer` 对应的文件 |
| <kbd>bx</kbd> | `scratch-buffer` 直接跳转到 `*scratch*` buffer |
| <kbd>bz</kbd> | `bury-buffer` 退出当前 `buffer` 的显示，当前 `buffer` 未被 kill |

| 按键 | 命令与说明 |
| --------------- | --------------------------------------------------------- |
| <kbd>bj</kbd> | `bookmark-jump` 跳转至书签 |
| <kbd>bJ</kbd> | `bookmark-jump-other-window` 同上，不过是在另一窗口打开 |
| <kbd>bm</kbd> | `bookmark-set` 设置书签 |
| <kbd>bM</kbd> | `bookmark-set-no-overwrite` 同上，但是不会覆盖同名的书签 |
| <kbd>bd</kbd> | `bookmark-delete` 删除书签 |
| <kbd>bi</kbd> | `bookmark-insert` 插入书签的内容 |
| <kbd>bl</kbd> | `bookmark-bmenu-list` 打开书签列表 |
| <kbd>br</kbd> | `bookmark-rename` 重命名书签 |
| <kbd>bs</kbd> | `bookmark-save` 保存书签 |
| <kbd>bw</kbd> | `bookmark-write` 将书签保存至其他文件 |

与窗口相关的 `Leader` 键绑定如下:

| 按键 | 命令与说明 |
| --------------- | ---------------------------------------------------- |
| <kbd>w</kbd> | `evil-window-map` 窗口操作的前缀键 (等同于 `C-w`) |
| <kbd>wx</kbd> | `kill-buffer-and-window` 关闭当前 buffer 并关闭窗口 |
| <kbd>wu</kbd> | `+transient-tab-bar-history` 撤销/恢复窗口布局 |
| <kbd>w-</kbd> | `split-window-vertically` 水平分割窗口 |
| <kbd>w/</kbd> | `split-window-horizontally` 垂直分割窗口 |

与标签页 (Tab) 相关的 `Leader` 键绑定如下:

| 按键 | 命令与说明 |
| --------------- | ------------------------------------------------------ |
| <kbd>tc</kbd> | `tab-bar-close-tab` 关闭当前标签页 |
| <kbd>tC</kbd> | `tab-bar-close-group-tabs` 关闭当前标签组的所有标签页 |
| <kbd>tg</kbd> | `tab-bar-change-tab-group` 修改当前标签页的分组 |
| <kbd>ti</kbd> | `tab-switcher` 切换标签页 |
| <kbd>tn</kbd> | `tab-bar-new-tab` 新建标签页 |
| <kbd>to</kbd> | `tab-bar-close-other-tabs` 关闭其他标签页 |
| <kbd>tt</kbd> | `tab-bar-switch-to-tab` 切换到指定的标签页 |
| <kbd>t'</kbd> | `tab-bar-switch-to-recent-tab` 切换到最近访问的标签页 |
| <kbd>tr</kbd> | `tab-bar-rename-tab` 重命名当前标签页 |

与项目相关的 `Leader` 键绑定如下:

| 按键 | 命令与说明 |
| -------------- | ------------------------------------------------------------------- |
| <kbd>p</kbd> | `projectile-command-map` 项目管理前缀键 (功能与下文的 `C-c p` 相同) |

打开其他程序的 `Leader` 键绑定:

| 按键 | 命令与说明 |
| --------------- | -------------------------------------- |
| <kbd>ot</kbd> | `ansi-term` 打开 `ansi-term` |
| <kbd>oe</kbd> | `eshell` 打开 `eshell` |
| <kbd>os</kbd> | `shell` 打开 `shell` |
| <kbd>ol</kbd> | `org-store-link` 存储URL |
| <kbd>oc</kbd> | `org-capture` 随时记录一些想法、URL等 |

打开一些看起来像是独立的应用:

| 按键 | 命令与说明 |
| --------------- | ------------------ |
| <kbd>aa</kbd> | `org-agenda` 日程 |
| <kbd>ac</kbd> | `calendar` 日历 |
| <kbd>ag</kbd> | `gnus` 查看新闻组 |
| <kbd>ai</kbd> | `rcirc` 上 IRC |

搜索相关的 `Leader` 键绑定:

| 按键 | 命令与说明 |
| --------------- | ----------------------- |
| <kbd>si</kbd> | `imenu` |
| <kbd>sj</kbd> | `evil-show-jumps` |
| <kbd>sm</kbd> | `evil-show-marks` |
| <kbd>sr</kbd> | `evil-show-registers` |
| <kbd>sp</kbd> | `consult-ripgrep` |
| <kbd>ss</kbd> | `consult-line` |

与代码相关的 `Leader` 键绑定:

| 按键 | 命令与说明 |
| --------------- | ----------------------------------------------------------- |
| <kbd>cd</kbd> | `rmsbolt-compile` 查看编译器的输出，如汇编、IR表示 |
| <kbd>cc</kbd> | `compile` 编译 |
| <kbd>cC</kbd> | `recompile` 重新编译 |
| <kbd>ck</kbd> | `kill-compilation` 打断当前的编译过程 |
| <kbd>cl</kbd> | `+switch-to-compilation` 快速切换到 `*compilation*` buffer |
| <kbd>cw</kbd> | `delete-trailing-whitespace` 删除行末空白字符 |
| <kbd>cx</kbd> | `quickrun` 快速运行当前程序 |

## Emacs

| 按键 | 命令与说明 |
| -------------------- | ----------------------------------------- |
| <kbd>M-\`</kbd> | `eshell-toggle` 打开或切换 Eshell 弹窗 |
| <kbd>M-;</kbd> | `comment-or-uncomment` 注释与反注释 |
| <kbd>C-c '</kbd> | 通过 `separedit` 在注释中快乐地写代码 |
| <kbd>C-c p</kbd> | `projectile` 调用前缀 |
| <kbd>C-x g</kbd> | 呼出 `magit` |
| <kbd>M-g M-l</kbd> | 调用 `avy-goto-line` |

Projectile 常用按键如下，按本机已安装包核对。第三方包未锁定版本，升级后可通过 `C-c p C-h` 或 Which-key 查看实际绑定。

| 按键 | 命令与说明 |
| ---------------------- | ---------------------------------------------------------------------- |
| <kbd>C-c p f</kbd> | `projectile-find-file` 在项目内查找其他文件 |
| <kbd>C-c p b</kbd> | `projectile-switch-to-buffer` 切换至其他 `buffer`(限定在本 `project` 下) |
| <kbd>C-c p c o</kbd> | `projectile-configure-project` 配置当前项目 |
| <kbd>C-c p c c</kbd> | `projectile-compile-project` 编译当前项目 |
| <kbd>C-c p c r</kbd> | `projectile-run-project` 运行当前项目 |
| <kbd>C-c p c t</kbd> | `projectile-test-project` 测试当前项目 |
| <kbd>C-c p p</kbd> | `projectile-switch-project` 切换至其他项目 |
| <kbd>C-c p s r</kbd> | `projectile-ripgrep` 使用 `ripgrep` 来搜索当前项目内的文本。 |

Flycheck 常用按键：

| 按键 | 命令与说明 |
| -------------------- | ------------------------------------------- |
| <kbd>C-c ! l</kbd> | `flycheck-list-errors` 列出所有 `lint` 错误 |
| <kbd>C-c ! n</kbd> | `flycheck-next-error` 下一个 `lint` 错误 |
| <kbd>C-c ! p</kbd> | `flycheck-previous-error` 上一个 `lint` 错误 |

完整绑定见 [init-evil.el](lisp/init-evil.el)。`C-c h` 下有两个 Transient 菜单：

| 按键 | 功能 |
| --- | --- |
| `C-c h t` | `background-opacity-menu`，调整背景透明度 |
| `C-c h o` | `scroll-other-window-menu`，滚动另一个窗口 |

# 通用开发设置

- Projectile 管理项目，`C-c p` 和 `SPC p` 为项目命令前缀。
- 默认在 `prog-mode` 启用 Company、Flycheck，并调用 `lsp-deferred`；能否建立 LSP 连接取决于客户端支持和语言服务器是否可用。Eglot 配置保留在文件中，但处于禁用状态。
- Flycheck 在保存和启用模式时检查；Magit 的提交编辑启用 Flyspell。
- `hl-todo` 高亮 TODO 等标记，Whitespace 显示行末空白，Hideshow 提供代码折叠。
- Magit 管理 Git，`diff-hl` 显示变更标记，Ediff 结束后恢复窗口布局。
- `rmsbolt` 查看本地编译器输出，Quickrun 运行代码，Citre 提供独立的 ctags 导航入口。
- `C-c b` 打开 DevDocs；`C-c c j` 调用 Citre 跳转。相关文档集和 tags 需要自行准备。
- Tempo 提供代码片段，SPDX 提供许可证头模板，可通过 `tempo-expand-if-complete` 或 `hippie-expand` 展开。

| LSP 按键 | 功能 |
| --- | --- |
| `C-c l` | LSP 命令前缀 |
| `C-c f` | 格式化区域 |
| `C-c d` | 查看当前位置的说明 |
| `C-c a` | 执行代码操作 |
| `C-c r` | 重命名符号 |
| `C-c l g h` | 显示调用层级，依赖本地 `lsp-hierarchy` |

# 语言支持

## C/C++ 与 CMake

使用 `cc-mode`，优先选择 PATH 中的 `clangd`；找不到时尝试将 `ccls` 配置为服务器。Clangd 参数启用了后台索引和 clang-tidy，具体设置见 [init-cpp.el](lisp/lang/init-cpp.el)。

包含 C/C++ 的 Tempo 模板，以及头文件、源文件、`_test.cpp` 和 `_benchmark.cpp` 之间的 sibling 查找规则。`SPC f o` 调用 `find-sibling-file`。

CMake 使用 `cmake-mode`、`cmake-font-lock` 和专用 Company 后端，`C-c d` 查看 CMake 帮助；另有 `lib` 等 Tempo 模板。LLVM IR 和 TableGen 模式需要按前文手工安装。

## Rust

使用 `rust-mode`，调整 `where` 子句缩进并加载可选库。使用 LSP 时需要准备 `rust-analyzer`；配置禁用了其 `unresolved-extern-crate` 诊断。

`rust-load-optional-libraries` 会加载 rust-mode 自带的 Cargo、编译和 rustfmt 等辅助库；使用这些命令需要对应的 Rust 工具。当前配置没有启用保存时自动格式化。

## OCaml

使用 Tuareg 编辑，进入 `tuareg-mode` 时启用 Merlin，并调用 `ocamlmerlin`。检测到 `ocp-indent` 或 `dune` 可执行文件时启用相关配置。

需要确保可执行文件位于 PATH 中，且 `merlin.el`、`ocp-indent.el`、`dune.el` 等 Lisp 库可被 Emacs 找到。配置不会通过 ELPA 自动安装这些包；安装来源取决于所用的系统或 OCaml 环境。LSP 仍遵循通用的 `lsp-deferred` 设置。

## Haskell

使用 `haskell-mode`，启用缩进和 `haskell-doc-mode`，关闭部分自动建议。没有语言专属的 LSP 排除规则，也没有指定服务器；需要 LSP 时另行准备对应服务器。

## Python、Emacs Lisp 与其他模式

- Python 使用内置 `python-mode`，交互式解释器按项目隔离。
- Emacs Lisp 中 `C-c C-c` 将表达式求值结果插入为注释；`SPC m e b` 求值整个 buffer，`SPC m e d` 求值当前定义，`SPC m g f` 查找函数定义。
- Shell、Bazel、YAML、Graphviz、ANTLR、Bison/Flex 等也有模式配置。详见 [init-dev.el](lisp/init-dev.el) 和 [lisp/lang/](lisp/lang/)。
- Tree-sitter 配置目前列出了 C、C++、JSON 的 grammar 来源；没有统一启用 `*-ts-mode` 的重映射，也不会自动安装 grammar。

# 写作与日常工具

## Org 与 Markdown

Org 默认使用 `~/.org/`：`tasks.org` 用于日程，`diary.org` 用于日记，`capture.org` 用于捕获笔记。首次使用时准备相应目录和文件，或修改 [init-org.el](lisp/init-org.el)。

任务状态包括 `TODO`、`HOLD`、`WIP`、`WAIT`、`DONE` 和 `CANCELLED`，并启用任务与复选框依赖。Capture 模板包括当天任务（`tt`）、收件箱（`ti`）、邮件任务（`tm`）和笔记（`cn`）。Agenda 生成后同步到 `appt`，加载 Agenda 配置后还会定时刷新提醒。

常用 Org Localleader 绑定：

| 按键 | 功能 |
| --- | --- |
| `SPC m t` | 更改任务状态 |
| `SPC m s` / `SPC m d` | 设置计划时间 / 截止时间 |
| `SPC m r` | Refile |
| `SPC m c c` / `SPC m c C` | 开始 / 停止计时 |
| `SPC m b t` | Tangle 当前文件 |

Babel 启用 C、Dot、Emacs Lisp、Eshell、Python 和 Shell，执行代码块时不询问确认。代码编辑窗口中可按 `C-c C-c` 返回 Org。

Org 和 Markdown 启用视觉换行与 `valign` 表格对齐。README 文件使用 `gfm-mode`；`separedit` 通过 `C-c '` 在独立 buffer 中编辑代码注释。

## Shell 与 Dired

`M-反引号 ` 调用 Eshell 弹窗。Eshell 内的 `vi`、`vim`、`nvim` 都映射到 `find-file`；`bat` 用语法高亮显示文件，`f` 查找文件，`z` 从目录历史中切换，`rg` 搜索结果可用于导航。

Dired 中 `C-c C-e` 进入 Wdired 编辑文件名，`(` 切换详细信息；配置还隐藏部分缓存文件，并使用 `diredfl` 提供高亮。

## AI 辅助

[init-gpt.el](lisp/init-gpt.el) 配置了 gptel 的 DeepSeek 后端，通过 `auth-source` 获取 API key；模型和请求选项以该文件为准。使用前需要配置自己的凭据。

可通过 `M-x gptel` 打开对话，默认使用 Org，支持流式输出、自动滚动和分支上下文。`C-c C-g` 中止请求。另有 `thinking` 和 `coding` 两个预设。

Magit 加载后可调用 `M-x gptel-commit`，将**暂存区 diff** 发送给配置的后端，生成 Conventional Commits 风格的提交说明。该命令不执行 Git 提交。

## 其他工具

- Gnus 和 rcirc：邮件、新闻组与 IRC；按需调整个人设置。
- Atomic Chrome：启动时开启服务，供浏览器扩展将文本交给 Emacs 编辑。
- `M-x fanyi-dwim`：词典查询；`M-x tldr`：查询命令示例。
- `C-c C-/`：Webjump 搜索入口；其中天气入口使用 Xwidget WebKit，需要相应构建支持。
- macOS：从 Shell 导入环境变量，设置修饰键、标题栏和 Emoji 字体；Option 映射为 Hyper，Command 映射为 Meta。

# 截图

以下为历史截图，主题、包和界面细节可能与当前配置不同。

![dashboard](https://user-images.githubusercontent.com/4024656/83343041-46cbc480-a328-11ea-800d-6cd88540186e.png
"Dashboard")

![magit_markdown](https://user-images.githubusercontent.com/4024656/83343152-30723880-a329-11ea-90ff-ec5f3b7d504a.png
"Magit and markdown")

![cpp_company](https://user-images.githubusercontent.com/4024656/83343182-965ec000-a329-11ea-862c-a0b25305a1b4.png
"cc-mode and company")

# FAQ

## [dashboard][dashboard] 图标显示异常

运行 `M-x nerd-icons-install-fonts` 安装图标字体，再检查当前 Emacs 是否能找到该字体。

## 更新时提示对应包版本不存在

如果您在使用国内镜像源时出现这个问题，多数情况都是由镜像源同步不一致导致的，可以切换成上游来规避这个问题。

``` elisp
(setq package-archives
      '(("melpa"  . "https://melpa.org/packages/")
        ("gnu"    . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
```

## Emacs 配置挂了

先使用 `emacs --debug-init` 查看启动错误。也可以加载 [init-mini.el](init-mini.el)，将问题缩小到基础配置：

``` bash
emacs -Q -l ~/.emacs.d/init-mini.el
```

`init-mini.el` 仍会初始化已安装的包、加载 `init-base.el` 和 `custom.el`，不是完全隔离的环境。若要排除所有个人配置，使用 `emacs -Q`。

# 其他

欢迎通过 Issue 反馈问题或建议。提交启动问题时，请附上 Emacs 版本、操作系统和 `--debug-init` 的错误信息。

[dashboard]: https://github.com/emacs-dashboard/emacs-dashboard/
[evil-snipe]: https://github.com/hlissner/evil-snipe
