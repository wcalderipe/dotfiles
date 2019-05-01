;; Take a look at the recipes available to el-get at:
;; ~/.emacs.d/el-get/el-get/recipes/

;; Small utility functions that allow for fish-style trunctated
;; directories in eshell and for example modeline. Force install from
;; ELPA. The original git repository
;; (https://github.com/c0001/shrink-path.el) no longer exists.
(el-get-bundle shrink-path :type elpa)

;; Default theme.
;; http://kippura.org/zenburnpage
(el-get-bundle color-theme-zenburn :checkout "66a3d09")

;; A minimal and modern mode-line.
(el-get-bundle doom-modeline :checkout "6f189b3")

;; The long lost Emacs string manipulation library.
;; Dependency of the deadgrep package.
(el-get-bundle s :checkout "03410e6")

;; YASnippet is a template system for Emacs. It allows you to type an
;; abbreviation and automatically expand it into function templates.
;; The snippet syntax is inspired from TextMate's syntax.
(el-get-bundle yasnippet :checkout "1d96da2")

;; Gives Ivy the ability to show recently used M-x commands.
(el-get-bundle DarwinAwardWinner/amx :checkout "4b08edb")

;; Modular in-buffer completion framework.
(el-get-bundle company-mode :checkout "20fe015")

;; Hydra can be used to tie related commands into a family of short
;; bindings with a common prefix.
(el-get-bundle hydra :checkout "9c4a471")

;; Swap buffers easily (great combo with the hydra package).
(el-get-bundle buffer-move :checkout "cb517ec")

;; Handles copy from/to the system clipboard.
(el-get-bundle emacsmirror/xclip :checkout "6d62531")

;; Map pairs of simultaneously pressed keys to commands.
(el-get-bundle emacsorphanage/key-chord :checkout "72443e9")

;; Extensions to Dired.
(el-get-bundle emacsmirror/dired-plus :checkout "bdd208e")

;; Editorconfig plugin for Emacs.
;; EditorConfig helps maintain consistent coding styles for multiple developers
;; working on the same project across various editors and IDEs.
(el-get-bundle editorconfig :checkout "898998d")

;; Displays available keybindings. Amazing package for discoverability.
(el-get-bundle which-key :checkout "9c5922e")

;; Minor mode that keeps your code always indented. It reindents after
;; every change, making it more reliable than electric-indent-mode.
(el-get-bundle aggressive-indent-mode :checkout "3803f24")

;; Magit is an interface to the version control system Git.
;; *Note*: The magit recipe runs a make target that requires "makeinfo" in your
;; system. In macOS 10.14 it's already installed, but in Linux you may need to
;; install the "texinfo" package.
(el-get-bundle magit/transient :checkout "328298f" :compile "lisp/" :load-path "lisp/")
(el-get-bundle magit-popup     :checkout "4250c3a")
(el-get-bundle magit           :checkout "d24296c")

;; Basic file type modes
(el-get-bundle groovy-emacs-mode   :checkout "d1f2474")
(el-get-bundle davazp/graphql-mode :checkout "ba59525")
(el-get-bundle markdown-mode       :checkout "53db081")
(el-get-bundle json-mode           :checkout "0e819e5")
(el-get-bundle skuro/plantuml-mode :checkout "366ecb6")
(el-get-bundle dockerfile-mode     :checkout "7223d92")
(el-get-bundle yaml-mode           :checkout "40067a1")

;; Evil emulates the main features of Vim.
(el-get-bundle evil :checkout "297b8f3"
  ;; Variables that must be set before evil is required.
  ;; https://stackoverflow.com/questions/14302171/ctrlu-in-emacs-when-using-evil-key-bindings
  :before (setq evil-want-C-u-scroll t
                ;; Do not load evil keybindings, because we'll use
                ;; from the evil-collection package.
                evil-want-keybinding nil))

;; Extra evil.
(el-get-bundle evil-magit                 :checkout "256614f" :load ("evil-magit.el"))
(el-get-bundle evil-surround              :checkout "9e445b7")
(el-get-bundle evil-leader                :checkout "39f7014")
(el-get-bundle evil-nerd-commenter        :checkout "72d4aac")
(el-get-bundle luxbock/evil-cleverparens  :checkout "8c45879")
(el-get-bundle emacs-evil/evil-collection :checkout "b067961")

;; A global minor mode with support for Vim/Tmux tabs (with certain
;; things missing, like automatic renumbering).
(el-get-bundle wasamasa/eyebrowse :checkout "a11c904")

(el-get-bundle paren-face         :checkout "a45d111")
(el-get-bundle paredit            :checkout "acbe10f")
(el-get-bundle smartparens        :checkout "4d15710")

;; If you have issues installing swiper in macOS you may need to
;; upgrade the old makeinfo tool.
;; https://github.com/abo-abo/swiper/issues/457
(el-get-bundle swiper             :checkout "2a02343" :load ("swiper.el" "ivy-hydra.el"))
(el-get-bundle projectile         :checkout "a6ecae5")
(el-get-bundle counsel-projectile :checkout "d64e527")
(el-get-bundle Yevgnen/ivy-rich   :checkout "e24f580")

;; Javascript/Typescript.
(el-get-bundle js2-mode :checkout "ed955e7")
(el-get-bundle tide     :checkout "b64f7b5")
(el-get-bundle mocha    :checkout "33e1b52")

;; Clojure and lots of Cider dependencies.
;; *Note*: Cider will connect to marmalade. Make sure your Emacs 26.x is
;; compiled with gnutls. The Emacs configure step (for Linux) requires
;; "pkg-config" to be installed in order to correctly detect the gnutls
;; version.
(el-get-bundle remvee/emacs-rails :checkout "45192ac" :load "inflections.el")
(el-get-bundle edn                :checkout "be9e32d")
(el-get-bundle clojure-mode       :checkout "4387cb8")
(el-get-bundle cider              :checkout "62134b4")
(el-get-bundle clj-refactor       :checkout "f5f44fd")
(el-get-bundle cider-hydra        :checkout "5956c39")
(el-get-bundle clojure-snippets   :checkout "6068dca")

;; Fast search interface using Ripgrep.
(el-get-bundle Wilfred/deadgrep :checkout "8b87ac1")

;; On the fly syntax checking (used by linters for instance).
(el-get-bundle flycheck :checkout "407bcd2")

;; Correct words with flyspell via custom interface (ivy, helm, etc).
(el-get-bundle flyspell-correct :checkout "cf49283")

;; A Emacs tree plugin like NerdTree for Vim.
(el-get-bundle neotree :checkout "c2420a4")

;; A simple emacs package to restart emacs from within emacs.
(el-get-bundle restart-emacs :checkout "9aa90d3")

;; Displays tildes in the fringe on empty lines a la Vi.
(el-get-bundle syl20bnr/vi-tilde-fringe :checkout "f1597a8")

;; Toggle highlighting of the symbol at point.
(el-get-bundle highlight-symbol :checkout "7a789c7")

;; Easily adjust the font size in all Emacs frames.
(el-get-bundle purcell/default-text-scale :checkout "f425d37")

(el-get 'sync)
