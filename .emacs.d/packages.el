;; Take a look at the recipes available to el-get at:
;; ~/.emacs.d/el-get/el-get/recipes/

;; The long lost Emacs string manipulation library.
;; Dependency of the deadgrep package.
(el-get-bundle s :checkout "03410e6")

;; Modern API for working with files and directories.
(el-get-bundle rejeep/f.el :checkout "8191672")

;; A modern list library.
;; dash-functional has to be explicitly loaded because it only works
;; with Emacs 24+ for its lexical scope.
(el-get-bundle magnars/dash.el :checkout "77f3bf4" :load ("dash.el" "dash-functional.el"))

;; Small utility functions that allow for fish-style trunctated
;; directories in eshell and for example modeline. Force install from
;; ELPA. The original git repository
;; (https://github.com/c0001/shrink-path.el) no longer exists.
(el-get-bundle shrink-path :type elpa)

;; Default theme.
;; http://kippura.org/zenburnpage
(el-get-bundle color-theme-zenburn :checkout "b6090a1")

;; A minimal and modern mode-line.
(el-get-bundle doom-modeline :checkout "1c5520d")

;; A light that follows my cursor around so I don't lose it!
(el-get-bundle beacon :checkout "bde7818")

;; YASnippet is a template system for Emacs. It allows you to type an
;; abbreviation and automatically expand it into function templates.
;; The snippet syntax is inspired from TextMate's syntax.
(el-get-bundle yasnippet :checkout "e45e3de")

;; Gives Ivy the ability to show recently used M-x commands.
(el-get-bundle DarwinAwardWinner/amx :checkout "b46e77d")

;; Modular in-buffer completion framework.
(el-get-bundle company-mode :checkout "ad6ff0e")

;; Hydra can be used to tie related commands into a family of short
;; bindings with a common prefix.
(el-get-bundle hydra :checkout "acb6985")

;; Swap buffers easily (great combo with the hydra package).
(el-get-bundle buffer-move :checkout "cb517ec")

;; Handles copy from/to the system clipboard.
(el-get-bundle emacsmirror/xclip :checkout "9407231")

;; Map pairs of simultaneously pressed keys to commands.
(el-get-bundle emacsorphanage/key-chord :checkout "72443e9")

;; Editorconfig plugin for Emacs.
;; EditorConfig helps maintain consistent coding styles for multiple developers
;; working on the same project across various editors and IDEs.
(el-get-bundle editorconfig :checkout "df102f4")

;; Displays available keybindings. Amazing package for discoverability.
(el-get-bundle which-key :checkout "187ac0e")

;; Minor mode that keeps your code always indented. It reindents after
;; every change, making it more reliable than electric-indent-mode.
(el-get-bundle aggressive-indent-mode :checkout "3803f24")

;; Magit is an interface to the version control system Git.
;; *Note*: The magit recipe runs a make target that requires "makeinfo" in your
;; system. In macOS 10.14 it's already installed, but in Linux you may need to
;; install the "texinfo" package.
(el-get-bundle magit/transient :checkout "4ef8229" :compile "lisp/" :load-path "lisp/")
(el-get-bundle magit-popup     :checkout "4250c3a")
(el-get-bundle magit           :checkout "b9c5ed1")

;; Basic file type modes
(el-get-bundle davazp/graphql-mode :checkout "74ee772")
(el-get-bundle markdown-mode       :checkout "115f77d")
(el-get-bundle json-mode           :checkout "0e819e5")
(el-get-bundle dockerfile-mode     :checkout "ed73e82")
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
(el-get-bundle evil-magit                 :checkout "e2fec58" :load ("evil-magit.el"))
(el-get-bundle evil-surround              :checkout "5ad01df")
(el-get-bundle evil-leader                :checkout "39f7014")
(el-get-bundle evil-nerd-commenter        :checkout "acc0f75")
(el-get-bundle luxbock/evil-cleverparens  :checkout "8c45879")
(el-get-bundle emacs-evil/evil-collection :checkout "4ed8cea")

;; A global minor mode with support for Vim/Tmux tabs (with certain
;; things missing, like automatic renumbering).
(el-get-bundle wasamasa/eyebrowse :checkout "52e1609")

(el-get-bundle paren-face         :checkout "a45d111")
(el-get-bundle paredit            :checkout "acbe10f")
(el-get-bundle smartparens        :checkout "0849801")

;; If you have issues installing swiper in macOS you may need to
;; upgrade the old makeinfo tool.
;; https://github.com/abo-abo/swiper/issues/457
(el-get-bundle swiper             :checkout "2ce81eb" :load ("swiper.el" "ivy-hydra.el"))
(el-get-bundle projectile         :checkout "4ca39e9")
(el-get-bundle counsel-projectile :checkout "fb19569")
(el-get-bundle Yevgnen/ivy-rich   :checkout "16af798")

;; Javascript/Typescript.
(el-get-bundle js2-mode :checkout "ae9fea8")
(el-get-bundle tide     :checkout "64eb602")
(el-get-bundle mocha    :checkout "33e1b52")
(el-get-bundle web-mode :checkout "a31eb85")

;; Clojure and lots of Cider dependencies.
;; *Note*: Cider will connect to marmalade. Make sure your Emacs 26.x is
;; compiled with gnutls. The Emacs configure step (for Linux) requires
;; "pkg-config" to be installed in order to correctly detect the gnutls
;; version.
(el-get-bundle remvee/emacs-rails :checkout "45192ac" :load "inflections.el")
(el-get-bundle edn                :checkout "be9e32d")
(el-get-bundle emacsmirror/queue  :checkout "3d5746b")
(el-get-bundle clojure-mode       :checkout "a9cbe6a")
(el-get-bundle cider              :checkout "62134b4")
(el-get-bundle clj-refactor       :checkout "3edc678")
(el-get-bundle cider-hydra        :checkout "5956c39")
(el-get-bundle clojure-snippets   :checkout "6068dca")

;; Fast search interface using Ripgrep.
(el-get-bundle Wilfred/deadgrep :checkout "3f372c1")

;; On the fly syntax checking (used by linters for instance).
(el-get-bundle flycheck :checkout "47174a1")

;; Correct words with flyspell via custom interface (ivy, helm, etc).
(el-get-bundle flyspell-correct :checkout "a085207")

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

;; Extra font lock rules for a more colourful dired.
(el-get-bundle purcell/diredfl :checkout "9b2a899")

;; This package helps me configure Emacs GUI to use the same variables
;; set by zshell. For example, this is important to use FZF (fuzzy
;; finder).
(el-get-bundle purcell/exec-path-from-shell :checkout "3cfedb8")

;; Emacs Lisp
;; Show evaluation results in an overlay.
(el-get-bundle xiongtx/eros :checkout "dd89102")

;; Thesaurus front-end with pluggable backends.
(el-get-bundle synosaurus :checkout "bc26f5c")

;; This minor mode sets background color to strings that match color
;; names
(el-get-bundle emacsmirror/rainbow-mode :checkout "fb7c982")

(el-get 'sync)
