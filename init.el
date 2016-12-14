;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ~/.emacs.d/init.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; You can edit this file as you like!
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; no start up message
;;(setq inhibit-startup-screen t)

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

;;;;;;;;基本的なEmacs操作の設定
;; 複数行移動
;;http://blog.shibayu36.org/entry/2012/12/04/111221
(global-set-key "\M-n" (kbd "C-u 5 C-n"))
(global-set-key "\M-p" (kbd "C-u 5 C-p"))

;;行番号の表示
(global-linum-mode t)

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

;; volatile-highlights
(require 'volatile-highlights)
(volatile-highlights-mode t)

;;ふわっとeffect
;;http://qiita.com/takaxp/items/73d9f2fbd15debe1ce2c
(defun eval-after-autoload-if-found	
    (functions file &optional docstring interactive type after-body) 
  "Set up autoload and eval-after-load for FUNCTIONS iff. FILE has found."
  (let ((enabled t)
        (package nil))
    (when (boundp 'loading-packages)
      (dolist (package loading-packages)
        (when (and (stringp (car package)) (equal file (car package)))
          (unless (cdr package)
            (setq enabled nil)
            (message "--- A setting for `%s' was NOT loaded explicitly"
                     (car package))))))
    (when enabled ;; if disabled then return nil.
      (when (locate-library file)
        (mapc (lambda (func)
                (autoload func file docstring interactive type))
              (if (listp functions)
                  functions
                (list functions)))
        (when after-body
          (eval-after-load file `(progn ,@after-body)))
        t))))

(when (eval-after-autoload-if-found
       '(volatile-highlights-mode) "volatile-highlights" nil t nil
       '((set-face-attribute
          'vhl/default-face nil :foreground "#FF3333" :background "#FFCDCD")
         (volatile-highlights-mode t)

         ;; ふわっとエフェクトの追加（ペースト時の色 => カーソル色 => 本来色）
         (defun my:vhl-change-color ()
           (let
               ((next 0.2)
                (reset 0.5)
                (colors '("#F8D3D7" "#F2DAE1" "#EBE0EB" "#E5E7F5" "#DEEDFF")))
             (dolist (color colors)
               (run-at-time next nil
                            'set-face-attribute
                            'vhl/default-face
                            nil :foreground "#FF3333" :background color)
               (setq next (+ 0.05 next)))
             (run-at-time reset nil 'vhl/clear-all))
           (set-face-attribute 'vhl/default-face
                               nil :foreground "#FF3333"
                               :background "#FFCDCD"))

         (defun my:yank (&optional ARG)
           (interactive)
           (yank ARG)
           (my:vhl-change-color))
         ;(global-set-key (kbd "M-v") 'my:yank)
         (global-set-key (kbd "C-y") 'my:yank)

         (with-eval-after-load "org"
           (define-key org-mode-map (kbd "C-y")
             '(lambda () (interactive)
                (org-yank)
                (my:vhl-change-color))))))

  (add-hook 'org-mode-hook 'volatile-highlights-mode)
  (add-hook 'emacs-lisp-mode-hook 'volatile-highlights-mode))

;;オプション・コマンドキー設定
(when (eq system-type 'darwin)
  (setq ns-option-modifier (quote meta)))

;;任意のbuffer切替
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <down>")  'windmove-down)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <right>") 'windmove-right)

;;起動時のデフォルトwidows size設定
(setq initial-frame-alist
      (append (list
	       '(width . 180)
	       '(height . 80)
	       '(top . 0)
	       '(left . 0)
	       '(font . "VL Gothic-14")
	       )
	      initial-frame-alist))
(setq default-frame-alist initial-frame-alist)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;便利にするゾ！

;recentf
;http://keisanbutsuriya.hateblo.jp/entry/2015/02/15/174758
(require 'recentf)
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

;;flymake
;;http://d.hatena.ne.jp/nyaasan/20071216/p1
(require 'flymake)
(defun flymake-cc-init ()
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
         (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
    (list "g++" (list "-Wall" "-Wextra" "-fsyntax-only" local-file))))

(push '("\\.cc$" flymake-cc-init) flymake-allowed-file-name-masks)

(add-hook 'c++-mode-hook
          '(lambda ()
             (flymake-mode t)))

;;
;;pyflakes
;;http://ksknw.hatenablog.com/entry/2016/05/07/171239
;;(flycheck-mode t)
    ;;errorやwarningを表示する
    ;;(require 'flymake-python-pyflakes)
    ;;(flymake-python-pyflakes-load)

;;undo-tree
;;http://qiita.com/takc923/items/c3d64b55fc4f3a3b0838
(require 'undo-tree)
(global-undo-tree-mode t)
(global-set-key (kbd "M-/") 'undo-tree-redo)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Local Variables:
;; mode: emacs-lisp
;; buffer-file-coding-system: utf-8-unix
;; End:
(put 'scroll-left 'disabled nil)
