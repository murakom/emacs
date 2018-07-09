;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ~/.emacs.d/init.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; You can edit this file as you like!
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; no start up message
(setq inhibit-startup-screen t)

;; window mode setting
(when (eq window-system 'ns)
  ;; window size
  ;;(setq default-frame-alist
  ;;   (append
  ;;     '((width . 100) (height . 40))
  ;;      default-frame-alist))
  ;; Custom-thema
  (load-theme 'deeper-blue t)
  (enable-theme 'deeper-blue)
  ;; Transparency3
  (add-to-list
   'default-frame-alist
   '(alpha . (100 80))) ;; (alpha . (<active frame> <non active frame>))
  )

;; input special and control characters by "Option"
(setq ns-option-modifier 'none)

;;;;;;;;;;;; personal settings

;;add packages from MELPA and marmalede パッケージの読み込み
;;http://keisanbutsuriya.hateblo.jp/entry/2015/01/29/182251
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;;;;;;;;Emacsの基本設定
;; ツールバーを非表示
(tool-bar-mode -1)
;; メニューバーを非表示
(menu-bar-mode -1)
;;; タイトルバーの表示 - ファイル名 <絶対パス>
(setq frame-title-format "%b <%f>")

;;; *.~ とかのバックアップファイルを作らない
(setq make-backup-files nil)
;;; .#* とかのバックアップファイルを作らない
(setq auto-save-default nil)

;;フォント
;; Basic font
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:foundry "Source Code Pro" :family "Source Code Pro"))))
 '(trailing-whitespace ((t (:foreground "dark blue" :underline t))))
 '(whitespace-empty ((t (:foreground "firebrick" :underline t))))
 '(whitespace-indentation ((t (:foreground "firebrick" :underline t))))
 '(whitespace-newline ((t (:foreground "darkgray" :underline t :weight normal))))
 '(whitespace-space ((t (:background "gray60" :foreground "darkgray" :underline t))))
 '(whitespace-tab ((t (:foreground "darkgray"))))
 '(whitespace-trailing ((t (:foreground "yellow" :underline t :weight bold)))))

;; Japanese font
(set-fontset-font t 'japanese-jisx0208 (font-spec :family "Hiragino Kaku Gothic ProN" ))
(set-frame-font "Hiragino Kaku Gothic ProN" 40)

;;テキストの折返し
(set-default 'truncate-lines nil)
(global-set-key (kbd "C-c t") 'toggle-truncate-lines)

;;スクロールバー
(require 'yascroll)
(global-yascroll-bar-mode t)

;; 複数行移動
;;http://blog.shibayu36.org/entry/2012/12/04/111221
(global-set-key "\M-n" (kbd "C-u 5 C-n"))
(global-set-key "\M-p" (kbd "C-u 5 C-p"))

;;単語の先頭・末尾に移動
(defun vim-word-forward()
  (interactive)
  (forward-word 2)
  (backward-word 1))
(global-set-key (kbd "M-<right>") 'vim-word-forward)

(global-set-key (kbd "M-%") 'vr/query-replace)

(defun vim-word-backward()
  (interactive)
  (backward-word 1))
(global-set-key (kbd "M-<left>") 'vim-word-backward)

;;一行コメントアウト
(defun one-line-comment-dwim ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (set-mark (point))
    (end-of-line)
    (comment-or-uncomment-region (region-beginning) (region-end))))
(global-set-key (kbd "C-:") 'one-line-comment-dwim)

;;行番号の表示
(global-linum-mode t)
;;; 一行あたりの文字数を指定してfill-region
(defun fill-region-with-N (num)
  ""
  (interactive "nfill-column value? ")
  (let ((fill-column num))
    (fill-region (region-beginning) (region-end)))
  )

;;ハイライト関係
;;http://keisanbutsuriya.hateblo.jp/entry/2015/02/01/162035
;(global-hl-line-mode t)                   ;; 現在行をハイライト
(show-paren-mode t)                       ;; 対応する括弧をハイライト
(setq show-paren-style 'mixed)            ;; 括弧のハイライトの設定。
(transient-mark-mode t)                   ;; 選択範囲をハイライト

;;インデントをハイライト
;;https://github.com/DarthFennec/highlight-indent-guides
(require 'highlight-indent-guides)
(setq highlight-indent-guides-method 'character)
(highlight-indent-guides-mode t)
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)

;;volatile-highlights
(require 'volatile-highlights)
(volatile-highlights-mode t)

;;スクロールのステップ量・マージン
(setq scroll-conservatively 1)
(setq next-screen-context-lines 5)
(setq scroll-preserve-screen-position t)
(setq scroll-margin 3)

;;カーソル位置を保持したままスクロール
(defun scroll-up-in-place (n)
  (interactive "p")
  (previous-line n)
  (scroll-down n))

(defun isearch-forward-on-cursor ()
  "Search the word on the cursor."
  (interactive)
  (forward-char)
  (backward-word)
  (isearch-forward))
(global-set-key "\C-s" 'isearch-forward-on-cursor)

(defun scroll-down-in-place (n)
  (interactive "p")
  (next-line n)
  (scroll-up n))
(global-set-key [M-up] 'scroll-up-in-place)
(global-set-key [M-down] 'scroll-down-in-place)

;;オプション・コマンドキー設定
(when (eq system-type 'darwin)
  (setq ns-option-modifier (quote meta)))
  ;; left command
  ;; (setq mac-command-modifier 'meta)
  ;; ;; left option
  ;; (setq mac-option-modifier 'alt)
  ;;
  ;; right command
  (setq mac-right-command-modifier 'meta)
  ;; ;; right option
  ;; (setq mac-right-option-modifier 'hyper)

;;方向キーでwindow切替
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <down>")  'windmove-down)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <right>") 'windmove-right)

;; カーソル位置から行頭まで削除する
(defun backward-kill-line (arg)
  "Kill chars backward until encountering the end of a line."
  (interactive "p")
  (kill-line 0))
;; C-S-kに設定
(global-set-key (kbd "C-S-k") 'backward-kill-line)

;;http://d.hatena.ne.jp/rubikitch/20100210/emacs
(defun other-window-or-split ()
  (interactive)
  (when (one-window-p)
    (split-window-horizontally))
  (other-window 1))

(global-set-key (kbd "C-,") 'other-window-or-split)

;;http://emacs.rubikitch.com/win-switch/
(require 'win-switch)
;;; 0.75秒間受け付けるタイマー
(setq win-switch-idle-time 1.0)
;;; 好きなキーを複数割り当てられる
;; ウィンドウ切り替え
(win-switch-set-keys '("k") 'up)
(win-switch-set-keys '("j") 'down)
(win-switch-set-keys '("h") 'left)
(win-switch-set-keys '("l") 'right)
(win-switch-set-keys '("o") 'next-window)
(win-switch-set-keys '("p") 'previous-window)
;; リサイズ
(win-switch-set-keys '("K") 'enlarge-vertically)
(win-switch-set-keys '("J") 'shrink-vertically)
(win-switch-set-keys '("H") 'shrink-horizontally)
(win-switch-set-keys '("L") 'enlarge-horizontally)
;; 分割
(win-switch-set-keys '("3") 'split-horizontally)
(win-switch-set-keys '("2") 'split-vertically)
(win-switch-set-keys '("0") 'delete-window)
;; その他
(win-switch-set-keys '(" ") 'other-frame)
(win-switch-set-keys '("u" [return]) 'exit)
(win-switch-set-keys '("\M-\C-g") 'emergency-exit)
;; C-x oを置き換える
(global-set-key (kbd "C-x o") 'win-switch-dispatch)

;;起動時のデフォルトwidows size設定
(setq initial-frame-alist
      (append (list
	       '(width . 160)
	       '(height . 80)
	       '(top . 10)
	       '(left . 0)
	       '(font . "VL Gothic-14")
	       )
	      initial-frame-alist))
(setq default-frame-alist initial-frame-alist)

;;ediff setting
;; コントロール用のバッファを同一フレーム内に表示
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
;; diffのバッファを上下ではなく左右に並べる
(setq ediff-split-window-function 'split-window-horizontally)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;便利にする

;recentf
;http://keisanbutsuriya.hateblo.jp/entry/2015/02/15/174758
(require 'recentf)
(recentf-mode 1)
;(setq recentf-save-file "~/.emacs.d/.recentf")
(setq recentf-max-saved-items 1000)            ;; recentf に保存するファイルの数
(setq recentf-exclude '(".recentf"))           ;; .recentf自体は含まない
(setq recentf-auto-cleanup 'never)             ;; 保存する内容を整理
(run-with-idle-timer 30 t '(lambda ()          ;; 30秒ごとに .recentf を保存
   (with-suppressed-message (recentf-save-list))))
;(require 'recentf-ext)

;; 透明度を変更するコマンド M-x set-alpha
;; http://qiita.com/marcy@github/items/ba0d018a03381a964f24
(defun set-al (alpha-num)
  "set frame parameter 'alpha"
  (interactive "nAlpha: ")
  (set-frame-parameter nil 'alpha (cons alpha-num '(90))))

;;sequential-command 同じコマンドを連続実行することで挙動を変える
;;http://d.hatena.ne.jp/rubikitch/20090219/sequential_command
(require 'sequential-command-config)
(global-set-key "\C-a" 'seq-home)
(global-set-key "\C-e" 'seq-end)
(when (require 'org nil t)
  (define-key org-mode-map "\C-a" 'org-seq-home)
  (define-key org-mode-map "\C-e" 'org-seq-end))
(define-key esc-map "u" 'seq-upcase-backward-word)
(define-key esc-map "c" 'seq-capitalize-backward-word)
(define-key esc-map "l" 'seq-downcase-backward-word)

;; Auto Complete
(require 'auto-complete-config)
(ac-config-default)
(add-to-list 'ac-modes 'text-mode)         ;; text-modeでも自動的に有効にする
(add-to-list 'ac-modes 'fundamental-mode)  ;; fundamental-mode
(add-to-list 'ac-modes 'org-mode)
(add-to-list 'ac-modes 'yatex-mode)
(ac-set-trigger-key "TAB")
(setq ac-use-menu-map t)       ;; 補完メニュー表示時にC-n/C-pで補完候補選択
(setq ac-use-fuzzy t)          ;; 曖昧マッチ
;; auto-complete の候補に日本語を含む単語が含まれないようにする
;; http://d.hatena.ne.jp/IMAKADO/20090813/1250130343
;; (defadvice ac-word-candidates (after remove-word-contain-japanese activate)
;;   (let ((contain-japanese (lambda (s) (string-match (rx (category japanese)) s))))
;;     (setq ad-return-value
;;           (remove-if contain-japanese ad-return-value))))
(global-set-key (kbd "C-x _") 'auto-complete-mode)

;;company mode
;;https://qiita.com/syohex/items/8d21d7422f14e9b53b17
;; (require 'company)
;; (global-company-mode) ; 全バッファで有効にする
;; (setq company-idle-delay 0.5) ; デフォルトは0.5
;; (setq company-minimum-prefix-length 2) ; デフォルトは4
;; (setq company-selection-wrap-around t) ; 候補の一番下でさらに下に行こうとすると一番上に戻る

;; (global-company-mode +1)
;; 自動補完を offにしたい場合は, company-idle-delayを nilに設定する
;; auto-completeでいうところの ac-auto-start にあたる.
;; (custom-set-variables
;;  '(company-idle-delay nil))
;; (set-face-attribute 'company-tooltip nil
;;                     :foreground "black" :background "lightgrey")
;; (set-face-attribute 'company-tooltip-common nil
;;                     :foreground "black" :background "lightgrey")
;; (set-face-attribute 'company-tooltip-common-selection nil
;;                     :foreground "white" :background "steelblue")
;; (set-face-attribute 'company-tooltip-selection nil
;;                     :foreground "black" :background "steelblue")
;; (set-face-attribute 'company-preview-common nil
;;                     :background nil :foreground "lightgrey" :underline t)
;; (set-face-attribute 'company-scrollbar-fg nil
;;                     :background "orange")
;; (set-face-attribute 'company-scrollbar-bg nil
;;                     :background "gray40")
;; (global-set-key (kbd "C-x _") 'global-company-mode)

;;helm
;;https://abicky.net/2014/01/04/170448/
(require 'helm-config)
(helm-mode 1)
(define-key global-map (kbd "M-x")     'helm-M-x)
(define-key global-map (kbd "C-x C-f") 'helm-find-files)
(define-key global-map (kbd "C-z") 'helm-recentf)
(define-key global-map (kbd "M-y")     'helm-show-kill-ring)
(define-key global-map (kbd "C-c i")   'helm-imenu)
(define-key global-map (kbd "C-x b")   'helm-buffers-list)
(define-key global-map (kbd "M-r")     'helm-resume)
(define-key global-map (kbd "<f1> h")   'helm-apropos)
(define-key helm-map (kbd "C-h") 'delete-backward-char)
(define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)
(define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
(define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)

;;helm-buffers-list のバッファ名の領域を広くとる
(setq helm-buffer-details-flag nil)

;;flymake
;;http://d.hatena.ne.jp/nyaasan/20071216/P1
;; (require 'flymake)

;; (defun flymake-cc-init ()
;;   (let* ((temp-file   (flymake-init-create-temp-buffer-copy
;;                        'flymake-create-temp-inplace))
;;          (local-file  (file-relative-name
;;                        temp-file
;;                        (file-name-directory buffer-file-name))))
;;     (list "g++" (list "-Wall" "-Wextra" "-fsyntax-only" local-file))))

;; (push '("\\.cpp$" flymake-cc-init) flymake-allowed-file-name-masks)

;; (add-hook 'c++-mode-hook
;;           '(lambda ()
;;              (flymake-mode t)))


;;pyflakes
;;http://ksknw.hatenablog.com/entry/2016/05/07/171239
;;(flycheck-mode t)
    ;;errorやwarningを表示する
    ;;(require 'flymake-python-pyflakes)
    ;;(flymake-python-pyflakes-load)

;;save guard
(require 'point-undo)
(global-set-key [f7] 'point-undo)
(global-set-key [M-f7] 'point-redo)

;;undo-tree
;;http://qiita.com/takc923/items/c3d64b55fc4f3a3b0838
(require 'undo-tree)
(global-undo-tree-mode t)
(global-set-key (kbd "M-/") 'undo-tree-redo)

;;multiple cursors
;;http://qiita.com/ongaeshi/items/3521b814aa4bf162181d
(require 'multiple-cursors)
(require 'smartrep)

(declare-function smartrep-define-key "smartrep")

(global-set-key (kbd "C-M-c") 'mc/edit-lines)
(global-set-key (kbd "C-M-r") 'mc/mark-all-in-region)

(global-unset-key "\C-t")

(smartrep-define-key global-map "C-t"
  '(("C-t"      . 'mc/mark-next-like-this)
    ("n"        . 'mc/mark-next-like-this)
    ("p"        . 'mc/mark-previous-like-this)
    ("m"        . 'mc/mark-more-like-this-extended)
    ("u"        . 'mc/unmark-next-like-this)
    ("U"        . 'mc/unmark-previous-like-this)
    ("s"        . 'mc/skip-to-next-like-this)
    ("S"        . 'mc/skip-to-previous-like-this)
    ("*"        . 'mc/mark-all-like-this)
    ("d"        . 'mc/mark-all-like-this-dwim)
    ("i"        . 'mc/insert-numbers)
    ("o"        . 'mc/sort-regions)
    ("O"        . 'mc/reverse-regions)))

;; ;;Emacs + LaTeX
;; ;;http://qiita.com/takuma0121/items/db4a8809438a4b9f837f
;; (setq TeX-default-mode 'japanese-latex-mode)

;; (setq japanese-LaTeX-default-style "jarticle")
;; (setq TeX-output-view-style '(("^dvi$" "." "xdvi '%d'")))
;; (setq preview-image-type 'dvipng)
;; (add-hook 'LaTeX-mode-hook (function (lambda ()
;;   (add-to-list 'TeX-command-list
;;     '("pTeX" "%(PDF)ptex %`%S%(PDFout)%(mode)%' %t"
;;      TeX-run-TeX nil (plain-tex-mode) :help "Run ASCII pTeX"))
;;   (add-to-list 'TeX-command-list
;;     '("pLaTeX" "%(PDF)platex %`%S%(PDFout)%(mode)%' %t"
;;      TeX-run-TeX nil (latex-mode) :help "Run ASCII pLaTeX"))
;;   (add-to-list 'TeX-command-list
;;     '("acroread" "acroread '%s.pdf' " TeX-run-command t nil))
;;   (add-to-list 'TeX-command-list
;;     '("pdf" "dvipdfmx -V 4 '%s' " TeX-run-command t nil))
;; )))

;; (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
;; (setq reftex-plug-into-AUCTeX t)

;; (setq TeX-auto-save t)
;; (setq TeX-parse-self t)
;; (setq-default TeX-master nil)

;; (add-hook 'LaTeX-mode-hook 'visual-line-mode)
;; ;; (add-hook 'LaTeX-mode-hook 'flyspell-mode)
;; (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
;; (add-hook 'TeX-mode-hook (lambda () (TeX-fold-mode 1)))

;; ;; Change key bindings
;; (add-hook 'reftex-mode-hook
;;  '(lambda ()
;;                (define-key reftex-mode-map (kbd "\C-cr") 'reftex-reference)
;;                (define-key reftex-mode-map (kbd "\C-cl") 'reftex-label)
;;                (define-key reftex-mode-map (kbd "\C-cc") 'reftex-citation)
;; ))

;; ;; 数式のラベル作成時にも自分でラベルを入力できるようにする
;; (setq reftex-insert-label-flags '("s" "sfte"))

;; ;; \eqrefを使う
;; (setq reftex-label-alist
;;       '(
;;         (nil ?e nil "\\eqref{%s}" nil nil)
;;         ))
;; ;;Aspell
;; (setq ispell-program- "aspell")

;;python indent
;;http://futurismo.biz/archives/2680
;; (require 'python)
;; (add-hook ‘python-mode-hook
;; ‘(lambda ()
;; (setq indent-tabs-mode nil)
;; (setq indent-level 4)
;; (setq python-indent 4)
;; (setq tab-width 4)))

;;YaTeXの夜明け
;;http://hikaru515.hatenablog.com/entry/2015/11/10/000000
;;https://texwiki.texjp.org/?YaTeX#x0d7daaa
;; ;;
;; ;; PATH
;; ;;
;; (setenv "PATH"
;;         (concat (getenv "PATH") ":/Library/TeX/texbin"))
;; ;;
;; ;; YaTeX
;; ;;
;; (autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
;; (setq auto-mode-alist
;;       (append '(("\\.tex$" . yatex-mode)
;;                 ("\\.ltx$" . yatex-mode)
;;                 ("\\.cls$" . yatex-mode)
;;                 ("\\.sty$" . yatex-mode)
;;                 ("\\.clo$" . yatex-mode)
;;                 ("\\.bbl$" . yatex-mode)) auto-mode-alist))
;; (setq YaTeX-inhibit-prefix-letter t)
;; (setq YaTeX-kanji-code nil)
;; (setq YaTeX-latex-message-code 'utf-8)
;; (setq YaTeX-use-LaTeX2e t)
;; (setq YaTeX-use-AMS-LaTeX t)
;; (setq YaTeX-dvi2-command-ext-alist
;;       '(("TeXworks\\|texworks\\|texstudio\\|mupdf\\|SumatraPDF\\|Preview\\|Skim\\|TeXShop\\|evince\\|okular\\|zathura\\|qpdfview\\|Firefox\\|firefox\\|chrome\\|chromium\\|Adobe\\|Acrobat\\|AcroRd32\\|acroread\\|pdfopen\\|xdg-open\\|open\\|start" . ".pdf")))
;; (setq tex-command "/Library/TeX/texbin/ptex2pdf -u -l -ot '-synctex=1'")
;; ;(setq tex-command "/Library/TeX/texbin/platex-ng -synctex=1")
;; ;(setq tex-command "/Library/TeX/texbin/pdflatex -synctex=1")
;; ;(setq tex-command "/Library/TeX/texbin/lualatex -synctex=1")
;; ;(setq tex-command "/Library/TeX/texbin/luajitlatex -synctex=1")
;; ;(setq tex-command "/Library/TeX/texbin/xelatex -synctex=1")
;; ;(setq tex-command "/Library/TeX/texbin/latexmk -e '$latex=q/uplatex %O -synctex=1 %S/' -e '$bibtex=q/upbibtex %O %B/' -e '$biber=q/biber %O --bblencoding=utf8 -u -U --output_safechars %B/' -e '$makeindex=q/upmendex %O -o %D %S/' -e '$dvipdf=q/dvipdfmx %O -o %D %S/' -norc -gg -pdfdvi")
;; ;(setq tex-command "/Library/TeX/texbin/latexmk -e '$latex=q/uplatex %O -synctex=1 %S/' -e '$bibtex=q/upbibtex %O %B/' -e '$biber=q/biber %O --bblencoding=utf8 -u -U --output_safechars %B/' -e '$makeindex=q/upmendex %O -o %D %S/' -e '$dvips=q/dvips %O -z -f %S | convbkmk -u > %D/' -e '$ps2pdf=q/ps2pdf %O %S %D/' -norc -gg -pdfps")
;; ;(setq tex-command "/Library/TeX/texbin/latexmk -e '$pdflatex=q/platex-ng %O -synctex=1 %S/' -e '$bibtex=q/upbibtex %O %B/' -e '$biber=q/biber %O --bblencoding=utf8 -u -U --output_safechars %B/' -e '$makeindex=q/upmendex %O -o %D %S/' -norc -gg -pdf")
;; ;(setq tex-command "/Library/TeX/texbin/latexmk -e '$pdflatex=q/pdflatex %O -synctex=1 %S/' -e '$bibtex=q/bibtex %O %B/' -e '$biber=q/biber %O --bblencoding=utf8 -u -U --output_safechars %B/' -e '$makeindex=q/makeindex %O -o %D %S/' -norc -gg -pdf")
;; ;(setq tex-command "/Library/TeX/texbin/latexmk -e '$pdflatex=q/lualatex %O -synctex=1 %S/' -e '$bibtex=q/upbibtex %O %B/' -e '$biber=q/biber %O --bblencoding=utf8 -u -U --output_safechars %B/' -e '$makeindex=q/upmendex %O -o %D %S/' -norc -gg -pdf")
;; ;(setq tex-command "/Library/TeX/texbin/latexmk -e '$pdflatex=q/luajitlatex %O -synctex=1 %S/' -e '$bibtex=q/upbibtex %O %B/' -e '$biber=q/biber %O --bblencoding=utf8 -u -U --output_safechars %B/' -e '$makeindex=q/upmendex %O -o %D %S/' -norc -gg -pdf")
;; ;(setq tex-command "/Library/TeX/texbin/latexmk -e '$pdflatex=q/xelatex %O -synctex=1 %S/' -e '$bibtex=q/upbibtex %O %B/' -e '$biber=q/biber %O --bblencoding=utf8 -u -U --output_safechars %B/' -e '$makeindex=q/upmendex %O -o %D %S/' -norc -gg -pdf")
;; (setq bibtex-command "/Library/TeX/texbin/latexmk -e '$latex=q/uplatex %O -synctex=1 %S/' -e '$bibtex=q/upbibtex %O %B/' -e '$biber=q/biber %O --bblencoding=utf8 -u -U --output_safechars %B/' -e '$makeindex=q/upmendex %O -o %D %S/' -e '$dvipdf=q/dvipdfmx %O -o %D %S/' -norc -gg -pdfdvi")
;; (setq makeindex-command "/Library/TeX/texbin/latexmk -e '$latex=q/uplatex %O -synctex=1 %S/' -e '$bibtex=q/upbibtex %O %B/' -e '$biber=q/biber %O --bblencoding=utf8 -u -U --output_safechars %B/' -e '$makeindex=q/upmendex %O -o %D %S/' -e '$dvipdf=q/dvipdfmx %O -o %D %S/' -norc -gg -pdfdvi")
;; (setq dvi2-command "/usr/bin/open -a Skim")
;; ;(setq dvi2-command "/usr/bin/open -a Preview")
;; ;(setq dvi2-command "/usr/bin/open -a TeXShop")
;; ;(setq dvi2-command "/Applications/TeXworks.app/Contents/MacOS/TeXworks")
;; ;(setq dvi2-command "/Applications/texstudio.app/Contents/MacOS/texstudio --pdf-viewer-only")
;; (setq tex-pdfview-command "/usr/bin/open -a Skim")
;; ;(setq tex-pdfview-command "/usr/bin/open -a Preview")
;; ;(setq tex-pdfview-command "/usr/bin/open -a TeXShop")
;; ;(setq tex-pdfview-command "/Applications/TeXworks.app/Contents/MacOS/TeXworks")
;; ;(setq tex-pdfview-command "/Applications/texstudio.app/Contents/MacOS/texstudio --pdf-viewer-only")
;; (setq dviprint-command-format "/usr/bin/open -a \"Adobe Acrobat Reader DC\" `echo %s | gsed -e \"s/\\.[^.]*$/\\.pdf/\"`")
;; (add-hook 'yatex-mode-hook
;;           '(lambda ()
;;              (auto-fill-mode -1)))
;; ;;
;; ;; RefTeX with YaTeX
;; ;;
;; ;(add-hook 'yatex-mode-hook 'turn-on-reftex)
;; (add-hook 'yatex-mode-hook
;;           '(lambda ()
;;              (reftex-mode 1)
;;              (define-key reftex-mode-map (concat YaTeX-prefix ">") 'YaTeX-comment-region)
;;              (define-key reftex-mode-map (concat YaTeX-prefix "<") 'YaTeX-uncomment-region)))

;; YaTeX
;;
(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
(setq auto-mode-alist
      (append '(("\\.tex$" . yatex-mode)
        ("\\.ltx$" . yatex-mode)
        ("\\.cls$" . yatex-mode)
        ("\\.sty$" . yatex-mode)
        ("\\.clo$" . yatex-mode)
        ("\\.bbl$" . yatex-mode)) auto-mode-alist))
(setq YaTeX-inhibit-prefix-letter t)
(setq YaTeX-kanji-code nil)
(setq YaTeX-latex-message-code 'utf-8)
(setq YaTeX-use-AMS-LaTeX t)
(setq YaTeX-dvi2-command-ext-alist
      '(("TeXworks\\|texworks\\|texstudio\\|mupdf\\|SumatraPDF\\|Preview\\|Skim\\|TeXShop\\|evince\\|okular\\|zathura\\|qpdfview\\|Firefox\\|firefox\\|chrome\\|chromium\\|Adobe\\|Acrobat\\|AcroRd32\\|acroread\\|pdfopen\\|xdg-open\\|open\\|start" . ".pdf")))
(setq tex-command "platex -synctex=1")
(setq dvi2-command "xdvi")
(when (equal system-type 'darwin)     ;; for Mac only
  (setq dvi2-command "/usr/bin/open -a Skim")
  (setq tex-pdfview-command "/usr/bin/open -a Skim"))
(setq bibtex-command "pbibtex")
(setq dviprint-command-format "dvipdfmx")


(add-hook 'yatex-mode-hook '(lambda () (auto-fill-mode -1))) ;; 自動で改行しない

;; preview commandの変更
;;http://tb-lab.hatenablog.jp/entry/2013/12/27/125423
(setq dvi2-command "open -a Preview")
(defvar YaTeX-dvi2-command-ext-alist
  '(("xdvi" . ".dvi")
    ("ghostview\\|gv" . ".ps")
    ("acroread\\|pdf\\|Preview\\|open" . ".pdf")))

;; texファイルを開くと自動でRefTexモード
;;
;(add-hook 'latex-mode-hook 'turn-on-reftex)
(add-hook 'yatex-mode-hook 'turn-on-reftex)
;; texファイルを開くと自動でRefTexモード
;;
;(add-hook 'latex-mode-hook 'turn-on-reftex)
(add-hook 'yatex-mode-hook 'turn-on-reftex)

;;http://d.hatena.ne.jp/takc923/
;; 何故かplatex等が使えなかったので(platex: command not foundとか表示される)pathを通す。
;; 必要なのは多分/usr/texbinだけだけど、コピペ元の設定をそのまま流用する。
(dolist (dir (list
              "/sbin"
              "/usr/sbin"
              "/bin"
              "/usr/bin"
              "/opt/local/bin"
              "/sw/bin"
              "/usr/local/bin"
              "/usr/texbin"
              (expand-file-name "~/bin")
              (expand-file-name "~/.emacs.d/bin")
              ))
 ;; PATH と exec-path に同じ物を追加します
 (when (and (file-exists-p dir) (not (member dir exec-path)))
   (setenv "PATH" (concat dir ":" (getenv "PATH")))
   (setq exec-path (append (list dir) exec-path))))

;;; org-mode
;;;http://d.hatena.ne.jp/tamura70/20100203/org
;; org-modeの初期化
(require 'org-install)
;; キーバインドの設定
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cr" 'org-remember)
;; 拡張子がorgのファイルを開いた時，自動的にorg-modeにする
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
;; org-modeでの強調表示を可能にする
(add-hook 'org-mode-hook 'turn-on-font-lock)
;; 見出しの余分な*を消す
(setq org-hide-leading-stars t)
;; org-default-notes-fileのディレクトリ
(setq org-directory "~/org/")
;; org-default-notes-fileのファイル名
(setq org-default-notes-file "notes.org")
;; TODO状態
(setq org-todo-keywords
      '((sequence "TODO(t)" "WAIT(w)" "|" "DONE(d)" "SOMEDAY(s)")))
;; DONEの時刻を記録
(setq org-log-done 'time)

;;行末の空白を削除
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;temporal setting
(global-set-key (kbd "C-.") 'delete-other-windows)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; Local Variables:
;; mode: emacs-lisp
;; buffer-file-coding-system: utf-8-unix
;; End:
(put 'scroll-left 'disabled nil)
(put 'narrow-to-region 'disabled nil)
