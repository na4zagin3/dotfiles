" vim:set ts=8 sts=2 sw=2 tw=0 fdm=marker: (この行に関しては:help modelineを参照)
"
" An example for a Japanese version vimrc file.
" 日本語版のデフォルト設定ファイル(vimrc) - Vim7用試作
"
" Last Change: 13-Jan-2015.
" Maintainer:  MURAOKA Taro <koron@tka.att.ne.jp>
"
" 解説:
" このファイルにはVimの起動時に必ず設定される、編集時の挙動に関する設定が書
" かれています。GUIに関する設定はgvimrcに書かかれています。
"
" 個人用設定は_vimrcというファイルを作成しそこで行ないます。_vimrcはこのファ
" イルの後に読込まれるため、ここに書かれた内容を上書きして設定することが出来
" ます。_vimrcは$HOMEまたは$VIMに置いておく必要があります。$HOMEは$VIMよりも
" 優先され、$HOMEでみつかった場合$VIMは読込まれません。
"
" 管理者向けに本設定ファイルを直接書き換えずに済ませることを目的として、サイ
" トローカルな設定を別ファイルで行なえるように配慮してあります。Vim起動時に
" サイトローカルな設定ファイル($VIM/vimrc_local.vim)が存在するならば、本設定
" ファイルの主要部分が読み込まれる前に自動的に読み込みます。
"
" 読み込み後、変数g:vimrc_local_finishが非0の値に設定されていた場合には本設
" 定ファイルに書かれた内容は一切実行されません。デフォルト動作を全て差し替え
" たい場合に利用して下さい。
"
" 参考:
"   :help vimrc
"   :echo $HOME
"   :echo $VIM
"   :version
"
scriptencoding utf-8

"---------------------------------------------------------------------------
" サイトローカルな設定($VIM/vimrc_local.vim)があれば読み込む。読み込んだ後に
" 変数g:vimrc_local_finishに非0な値が設定されていた場合には、それ以上の設定
" ファイルの読込を中止する。
" site-local {{{
if 1 && filereadable($VIM . '/vimrc_local.vim')
  unlet! g:vimrc_local_finish
  source $VIM/vimrc_local.vim
  if exists('g:vimrc_local_finish') && g:vimrc_local_finish != 0
    finish
  endif
endif
" site-local }}}

"---------------------------------------------------------------------------
" ユーザ優先設定($HOME/.vimrc_first.vim)があれば読み込む。読み込んだ後に変数
" g:vimrc_first_finishに非0な値が設定されていた場合には、それ以上の設定ファ
" イルの読込を中止する。
if 0 && exists('$HOME') && filereadable($HOME . '/.vimrc_first.vim')
  unlet! g:vimrc_first_finish
  source $HOME/.vimrc_first.vim
  if exists('g:vimrc_first_finish') && g:vimrc_first_finish != 0
    finish
  endif
endif

"---------------------------------------------------------------------------
" 日本語対応のための設定:
"
" ファイルを読込む時にトライする文字エンコードの順序を確定する。漢字コード自
" 動判別機能を利用する場合には別途iconv.dllが必要。iconv.dllについては
" README_w32j.txtを参照。ユーティリティスクリプトを読み込むことで設定される。
" source ~/.vim/encode_japan.vim
" メッセージを日本語にする (Windowsでは自動的に判断・設定されている)
" if !(has('win32') || has('mac')) && has('multi_lang')
"   if !exists('$LANG') || $LANG.'X' ==# 'X'
"     if !exists('$LC_CTYPE') || $LC_CTYPE.'X' ==# 'X'
"       language ctype ja_JP.eucJP
"     endif
"     if !exists('$LC_MESSAGES') || $LC_MESSAGES.'X' ==# 'X'
"       language messages ja_JP.eucJP
"     endif
"   endif
" endif
" " MacOS Xメニューの日本語化 (メニュー表示前に行なう必要がある)
" if has('mac')
"   set langmenu=japanese
" endif
" " 日本語入力用のkeymapの設定例 (コメントアウト)
" if has('keymap')
"   " ローマ字仮名のkeymap
"   "silent! set keymap=japanese
"   "set iminsert=0 imsearch=0
" endif
" " 非GUI日本語コンソールを使っている場合の設定
" if !has('gui_running') && &encoding != 'cp932' && &term == 'win32'
"   set termencoding=cp932
" endif
" japanese-encoding }}}

"---------------------------------------------------------------------------
" メニューファイルが存在しない場合は予め'guioptions'を調整しておく
" menu-option {{{
if 1 && !filereadable($VIMRUNTIME . '/menu.vim') && has('gui_running')
  set guioptions+=M
endif
" menu-option }}}

"---------------------------------------------------------------------------
" 検索の挙動に関する設定:
"
" finding-config {{{
" 検索時に大文字小文字を無視 (noignorecase:無視しない)
set ignorecase
" 大文字小文字の両方が含まれている場合は大文字小文字を区別
set smartcase
" finding-config }}}

"---------------------------------------------------------------------------
" 編集に関する設定:
"
" editing-config {{{
" タブの画面上での幅
set tabstop=4
" タブをスペースに展開しない (expandtab:展開する)
set noexpandtab
" 自動的にインデントする (noautoindent:インデントしない)
set autoindent
" バックスペースでインデントや改行を削除できるようにする
set backspace=2
" 検索時にファイルの最後まで行ったら最初に戻る (nowrapscan:戻らない)
set wrapscan
" 括弧入力時に対応する括弧を表示 (noshowmatch:表示しない)
set showmatch
" コマンドライン補完するときに強化されたものを使う(参照 :help wildmenu)
set wildmenu
" テキスト挿入中の自動折り返しを日本語に対応させる
set formatoptions+=mM
" 日本語整形スクリプト(by. 西岡拓洋さん)用の設定
let format_allow_over_tw = 1	" ぶら下り可能幅
" editing-config }}}

"---------------------------------------------------------------------------
" GUI固有ではない画面表示の設定:
"
" nogui-config {{{
" 行番号を表示 (number:表示)
set number
" 相対行番号を表示 (number:表示)
set relativenumber
" ルーラーを表示 (noruler:非表示)
set ruler
" タブや改行を表示 (list:表示)
set nolist
" どの文字でタブや改行を表示するかを設定
"set listchars=tab:>-,extends:<,trail:-,eol:<
" 長い行を折り返して表示 (nowrap:折り返さない)
set wrap
" 常にステータス行を表示 (詳細は:he laststatus)
set laststatus=2
" ステータス行の内容
set statusline=%f%m%r%h%w\ %Y\ %{&ff}\ %{&fenc}\ %k\%=\ =\%0B\ %0l,%0v/%p%%/%L
" コマンドラインの高さ (Windows用gvim使用時はgvimrcを編集すること)
set cmdheight=2
" コマンドをステータス行に表示
set showcmd
" タイトルを表示
set title
" インクリメンタルーチを有効
set incsearch
" 画面を黒地に白にする (次行の先頭の " を削除すれば有効になる)
"colorscheme evening " (Windows用gvim使用時はgvimrcを編集すること)
"colorscheme torte " (Windows用gvim使用時はgvimrcを編集すること)
" nogui-config }}}

"---------------------------------------------------------------------------
" ファイル操作に関する設定:
"
" バックアップファイルを作成しない (次行の先頭の " を削除すれば有効になる)
" file-config {{{
"set nobackup
" file-config }}}


"---------------------------------------------------------------------------
" ファイル名に大文字小文字の区別がないシステム用の設定:
"   (例: DOS/Windows/MacOS)
"
" filename-patch {{{
if filereadable($VIM . '/vimrc') && filereadable($VIM . '/ViMrC')
  " tagsファイルの重複防止
  set tags=./tags,tags
endif
" filename-patch }}}

"---------------------------------------------------------------------------
" コンソールでのカラー表示のための設定(暫定的にUNIX専用)
" color-patch {{{
" if has('unix') && !has('gui_running')
"   let uname = system('uname')
"   if uname =~? "linux"
"     set term=builtin_linux
"   elseif uname =~? "freebsd"
"     set term=builtin_cons25
"   elseif uname =~? "Darwin"
"     set term=beos-ansi
"   else
"     set term=builtin_xterm
"   endif
"   unlet uname
" endif
" color-patch }}}

"---------------------------------------------------------------------------
" コンソール版で環境変数$DISPLAYが設定されていると起動が遅くなる件へ対応
" display-patch {{{
if !has('gui_running') && has('xterm_clipboard')
  set clipboard=exclude:cons\\\|linux\\\|cygwin\\\|rxvt\\\|screen
endif
" display-patch }}}

"---------------------------------------------------------------------------
" プラットホーム依存の特別な設定

" plathome-dependent-patch {{{
" WinではPATHに$VIMが含まれていないときにexeを見つけ出せないので修正
if has('win32') && $PATH !~? '\(^\|;\)' . escape($VIM, '\\') . '\(;\|$\)'
  let $PATH = $VIM . ';' . $PATH
endif

if has('mac')
  " Macではデフォルトの'iskeyword'がcp932に対応しきれていないので修正
  set iskeyword=@,48-57,_,128-167,224-235
endif
" plathome-dependent-patch }}}

" FileType {{{
augroup MyGroup
  autocmd!
  autocmd FileType c,c++,c# :set shiftwidth=4
  autocmd FileType tex :set shiftwidth=4
  " schemeをgaucheに
  autocmd FileType scheme :let is_gauche=1
  autocmd BufNewFile,BufRead *.Rtex :setfiletype tex
  autocmd BufNewFile,BufRead *.tup :setfiletype tup
  autocmd BufNewFile,BufRead Tupfile :setfiletype tup
  autocmd BufReadCmd   *.epub      call zip#Browse(expand("<amatch>"))
  autocmd BufEnter *.md :syntax sync fromstart
augroup END
" FileType }}}

" enviroment {{{
" if has('win32')
"   let $CYGWINROOT=substitute($CYGWINROOT, "\\", "/", "")
"   set shell=bash
"   set shellcmdflag=-c
"   set makeef=
"   set shellslash
"   let $PATH=$CYGWINROOT.'/bin;'.$CYGWINROOT.'/usr/local/bin;'.$PATH
"   let $docmentpath='i:/mrty/Documents'
"   let $PLUGINROOT=$VIM
"   let qfixmemo_dir = "i:/howm"
" elseif has('unix')
"   set shell=bash
"   set shellcmdflag=-c
"   set makeef=
"   set shellslash
if hostname() == 'geirscoegul'
  let $PLUGINROOT=$HOME.'/vim-plugin'
  let qfixmemo_chenv_dir = '~/Documents/qfixmemo'
  let qfixmemo_dir = qfixmemo_chenv_dir
else
  let $PLUGINROOT=$HOME.'/vim-plugin'
  let qfixmemo_chenv_dir = '~/qfixmemo'
  let qfixmemo_dir = qfixmemo_chenv_dir
endif

"   if has('gui')
"     set ambiwidth=single
" 
" "    set guifont=Monospace\ 10
" "    set guifontwide=Kozuka\ Gothic\ Pro-VI\ Medium\ 10
" 
"     set guifont=Luxi\ Mono\ 10
"     set guifontwide=Kozuka\ Mincho\ Pro-VI\ Medium\ 10
" "
" "    set guifont=Courier\ Std\ Medium\ 10
" "    set guifont=Courier\ 10\ Pitch\ 10
" "    set guifontwide=Kozuka\ Gothic\ Pro-VI\ Medium\ 10
" 
" "    set guifont=clean\ 6
" "    set guifontwide=Kozuka\ Gothic\ Pro-VI\ Medium\ 8
"   endif
" endif
if has('mac')
  let $PATH='/Users/mrty/.cabal/bin:'.$PATH
  let $PATH='/Users/mrty/.opam/system/bin:'.$PATH
  let $PATH='/usr/local/share/npm/bin:'.$PATH
endif
" set fileencodings=utf-8,iso-2022-jp,euc-jp,cp932
set fileencodings=ucs-bom,utf-8,iso-2022-jp-2004,iso-2022-jp-3,iso-2022-jp,euc-jisx0213,euc-jp,eucjp-ms,Shift_JIS-2004,cp932
let g:mapleader = "<Bslash>"
" enviroment }}}

" howm {{{
" autocmd! QFixMRU BufLeave
" let g:QFixMRU_Disable = 1
" let g:disable_QFixMemoChEnv = 1
let QFixMRU_EntryMax     = 30
let g:QFixHowm_Convert = 0
let qfixmemo_fileencoding='utf-8'
let qfixmemo_fileformat='dos'
let qfixmemo_keywordfile= qfixmemo_chenv_dir . '/.howm-keys'

" howm-filetype {{{
"let howm_filename = '%Y/%m/%Y-%m-%d-%H%M%S.txt'
let qfixmemo_filename = '%Y/%m/%Y-%m-%d-%H%M%S.howm'
"let QFixHowm_FileType = 'markdown.howm_memo'
let QFixHowm_FileType = 'qfix_memo'
let QFixHowm_Title    = '='

let qfixmemo_template = [
  \'%TITLE% %TAG%',
  \'%DATE%',
  \''
\]
let QFixHowm_Template_mkd = [
  \"%TITLE% %TAG%",
  \"%DATE%",
  \''
\]
"mkdテンプレート(カーソル移動)
let QFixHowm_Cmd_NewEntry_mkd = '$a'
" howm-filetype }}}

"MRUを保存するファイル
let QFixMRU_Filename    = qfixmemo_chenv_dir.'/.howm-mru'
" MRUの基準ディレクトリ
let QFixMRU_RootDir   = qfixmemo_chenv_dir


"URIを開くコマンド
":help openuri

" howm-chenv.vim
"howmディレクトリの切替
nnoremap <silent> g,hh :echo qfixmemo_dir<CR>
nnoremap <silent> g,ha :call QFixMemoChEnv('',         'time', '=')<CR>
nnoremap <silent> g,hm :call QFixMemoChEnv('main',     'time', '=')<CR>
nnoremap <silent> g,ho :call QFixMemoChEnv('old',      'time', '=')<CR>
nnoremap <silent> g,hn :call QFixMemoChEnv('main-mkd', 'time', '#')<CR>
nnoremap <silent> g,hp :call QFixMemoChEnv('pc',       'time', '= [:pc]')<CR>
nnoremap <silent> g,hw :call QFixMemoChEnv('work',     'day',  '=')<CR>
" howm }}}

" QFixGrep {{{
let mygrepprg = 'grep'
let MyGrep_ShellEncoding = 'utf-8'
" QFixGrep }}}

" FuzzyFinder {{{
        let g:FuzzyFinderOptions = { 'Base':{}, 'Buffer':{}, 'File':{}, 'Dir':{}, 'MruFile':{}, 'MruCmd':{}, 'Bookmark':{}, 'Tag':{}, 'TaggedFile':{}}
        let g:FuzzyFinderOptions.Base.abbrev_map  = {
              \   '^v|' : map(filter(split(&runtimepath, ','), 'v:val !~ "after$"'), 'v:val . "/**/"'),
              \ }
        let g:FuzzyFinderOptions.MruFile.max_item = 300
        let g:FuzzyFinderOptions.MruCmd.max_item = 400
        nnoremap <silent> <C-n>      :FuzzyFinderBuffer<CR>
        nnoremap <silent> <C-p>      :FuzzyFinderFileWithCurrentBufferDir<CR>
        nnoremap <silent> <C-f><C-p> :FuzzyFinderFileWithFullCwd<CR>
        nnoremap <silent> <C-f>p     :FuzzyFinderFile
        nnoremap <silent> <C-j>      :FuzzyFinderMruFile<CR>
        nnoremap <silent> <C-k>      :FuzzyFinderMruCmd<CR>
        nnoremap <silent> <C-f><C-d> :FuzzyFinderDirWithCurrentBufferDir<CR>
        nnoremap <silent> <C-f>d     :FuzzyFinderDirWithFullCwd<CR>
        nnoremap <silent> <C-b>      :FuzzyFinderBookmark<CR>
        nnoremap <silent> <C-f><C-t> :FuzzyFinderTag<CR>
        nnoremap <silent> <C-f>t     :FuzzyFinderTag!<CR>
        noremap  <silent> g]         :FuzzyFinderTagWithCursorWord!<CR>
        nnoremap <silent> <C-f><C-g> :FuzzyFinderTaggedFile<CR>
        nnoremap <silent> <C-f><C-b> :FuzzyFinderAddBookmark<CR>
        vnoremap <silent> <C-f><C-b> :FuzzyFinderAddBookmarkAsSelectedText<CR>
        nnoremap <silent> <C-f><C-e> :FuzzyFinderEditInfo<CR>
        nnoremap <silent> <C-f><C-r> :FuzzyFinderRenewCache<CR>
" FuzzyFinder }}}
" outputz {{{
let g:outputz_secret_key="knBLJKqyaTkr"
let g:outputz_uri_function="outputz#default_uri_function"
let g:outputz_curl='curl'
let g:outputzdebugstring=""
" outputz }}}
" MinSCM {{{
set runtimepath+=$PLUGINROOT/minscm
" MinSCM }}}

        
" gtags {{{
:nmap <C-\><C-]> :GtagsCursor<CR>
" gtags }}}

" binary {{{
" vim -b : edit binary using xxd-format!
"augroup Binary
"  au!
"  au BufReadPre  *.bin let &bin=1
"  au BufReadPost *.bin if &bin | silent %!xxd -g 1
"  au BufReadPost *.bin set ft=xxd | endif
"  au BufWritePre *.bin if &bin | %!xxd -r
"  au BufWritePre *.bin endif
"  au BufWritePost *.bin if &bin | silent %!xxd -g 1
"  au BufWritePost *.bin set nomod | endif
"augroup END
" binary }}}

" encoding {{{
set encoding=utf-8
set fileencoding=utf-8
let &termencoding = &encoding
set langmenu=ja_JP.utf-8
" encoding }}}
" {{{ keymap
" Disable s and S
nnoremap s <nop>
nnoremap S <nop>
vnoremap s <nop>
nnoremap S <nop>
" }}} keymap
" TwitVim {{{
if has("perl")
  let twitvim_enable_perl = 1
endif
if has("python")
  let twitvim_enable_python = 1
endif
if has("ruby")
  let twitvim_enable_ruby = 1
endif
if has("tcl")
  let twitvim_enable_tcl = 1
endif


" TwitVim }}}
" haskell mode {{{
" use ghc functionality for haskell files
augroup MyHaskell
  " au Bufenter *.hs,*.lhs compiler ghc
  au BufRead,BufNewFile *.hs setl sw=2 expandtab
  au BufRead,BufNewFile *.lhs setl sw=2 expandtab
  au BufRead,BufNewFile *.cabal setl sw=2 expandtab
  au BufWritePost *.hs :GhcModCheckAndLintAsync
  au BufRead,BufNewFile *.hamlet  setf hamlet | setl expandtab
  au BufRead,BufNewFile *.cassius setf cassius | setl expandtab
  au BufRead,BufNewFile *.lucius  setf lucius | setl expandtab
  au BufRead,BufNewFile *.julius  setf julius | setl expandtab
augroup END
" configure browser for haskell_doc.vim
let g:haddock_browser = "chromium"
"let g:haddock_browser = "C:/Program Files/Opera/Opera.exe"
"let g:haddock_browser = "C:/Program Files/Mozilla Firefox/firefox.exe"
"let g:haddock_browser = "C:/Program Files/Internet Explorer/IEXPLORE.exe"
" haskell mode }}}
" Align {{{
" Alignを日本語環境で使用するための設定
let g:Align_xstrlen = 3
" Align }}}
" pandoc {{{
let g:pandoc_bibfiles=["$BIBLIO_DIR/computer.bib"]
" pandoc }}}
" tcvime {{{
let tcvime_enable = 1
if tcvime_enable
if has('keymap')
  let tcvime_keymap = 'tutcodep'
  set iminsert=0 imsearch=0
  imap <unique> <C-J> <Plug>TcvimeIEnableKeymap
  imap <silent> <unique> <C-L> <Plug>TcvimeIDisableKeymap
  imap <silent> <unique> <ESC> <ESC>:set imsearch=0<CR>
  " コントロールキーを伴わないモード切り替え: <Space>,でオンにする
  " imap <silent> <unique> , <C-G>u<C-R>=tcvime#EnableKeymapOrInsertChar(',',1)<CR>
  " <Space>;で後置型英字変換
  " imap <silent> <unique> ; <C-G>u<C-R>=tcvime#InputPostConvertAscii(';')<CR>
endif

" <Plug>TcvimeIEnableKeymap実行時にコールバックされる関数。
function OnTcvimeEnableKeymap()
  " <Space>で前置型交ぜ書き変換を開始するか、読みが無ければ' 'を挿入。
  " (lmapにすると、lmap有効時にfやtやrの後の<Space>が使用不可。(<C-R>=なので))
  "imap <silent> <Space> <C-G>u<Plug>TcvimeIConvOrSpace
  "imap <silent> <unique> <C-K>/ <Plug>TcvimeIAsciiStart
  nmap <silent> <unique> <C-K>k <Plug>TcvimeNKatakana
  nmap <silent> <unique> <C-K>h <Plug>TcvimeNHiragana
  nmap <silent> <unique> <C-K><Space> <Plug>TcvimeNConvert
  vmap <silent> <unique> <C-K>k <Plug>TcvimeVKatakana
  vmap <silent> <unique> <C-K>h <Plug>TcvimeVHiragana
  vmap <silent> <unique> <C-K>; <Plug>TcvimeVKanji2Seq
  vmap <silent> <unique> <C-K>z <Plug>TcvimeVSeq2Kanji
  vmap <silent> <unique> <C-K>, <Plug>TcvimeVShiftSeq
  " 後置型部首合成変換
  lmap <silent> ala <Plug>TcvimeIBushu
  " 前置型交ぜ書き変換の読み入力開始
  lmap <silent> alj <Plug>TcvimeIStart
  " 前置型交ぜ書き変換
  lmap <silent> al<Space> <Plug>TcvimeIConvOrStart
endfunction

" <Plug>TcvimeIDisableKeymap()実行時にコールバックされる関数。
function OnTcvimeDisableKeymap()
  silent! iunmap <Space>
  "silent! iunmap <C-K>/
  silent! nunmap <C-K>k
  silent! nunmap <C-K>h
  silent! nunmap <C-K><Space>
  silent! vunmap <C-K>k
  silent! vunmap <C-K>h
  silent! vunmap <C-K>;
  silent! vunmap <C-K>z
  silent! vunmap <C-K>,
  TcvimeCloseHelp
endfunction

" lmapのカスタマイズ用の関数。
" <Plug>TcvimeEnableKeymap実行中にtcvime#SetKeymap()からコールバックされる。
" (lmapのロード時に1回のみ実行されるようにするため、
" OnTcvimeEnableKeymap()と別関数)
function TcvimeCustomKeymap()
  " (TUT-Code用の例)
  " 後置型部首合成変換
  lmap <silent> ala <C-G>u<Plug>TcvimeIBushu
  " 前置型交ぜ書き変換の読み入力開始
  lmap <silent> alj <C-G>u<Plug>TcvimeIStart
  " 前置型交ぜ書き変換
  lmap <silent> al<Space> <C-G>u<Plug>TcvimeIConvOrStart
  " lmapオフ
  lmap <silent> a9 <C-G>u<Plug>TcvimeIDisableKeymap
  " 前置型英字変換の読み入力開始
  lmap <silent> a8 <Plug>TcvimeIAsciiStart
  " 前置型交ぜ書き変換
  lmap <silent> al<Space> <C-G>u<Plug>TcvimeIConvOrStart
  " 前置型交ぜ書き変換(活用する語として変換)
  lmap <silent> ali <C-G>u<Plug>TcvimeIKatuyo
  " 直前のカタカナ変換・交ぜ書き変換・部首合成変換の取り消し
  lmap <silent> alz <C-G>u<C-R>=tcvime#InputConvertUndo()<CR>
  " 直前の交ぜ書き変換を縮める
  lmap <silent> m> <C-G>u<Plug>TcvimeIShrink
  " 後置型ひらがな変換: 指定文字数をひらがな変換。0:カタカナが続く間
  lmap <silent> i0 <C-G>u<C-R>=tcvime#InputConvertHiragana(0)<CR>
  " 後置型カタカナ変換: 指定文字数をカタカナ変換。0:ひらがなが続く間
  lmap <silent> k0 <C-G>u<C-R>=tcvime#InputConvertKatakana(0)<CR>
  for i in range(1, 9)
    execute 'lmap <silent> k' . i "\<C-G>u\<C-R>=tcvime#InputConvertKatakana(" . i . ")\<CR>"
    " 後置型カタカナ変換: 指定文字数のひらがなを残してカタカナ変換
    execute 'lmap <silent> j' . i "\<C-G>u\<C-R>=tcvime#InputConvertKatakana(-" . i . ")\<CR>"
    " 直前のカタカナ変換を縮める
    execute 'lmap <silent> l' . i "\<C-G>u\<C-R>=tcvime#InputConvertKatakanaShrink(" . i . ")\<CR>"
    " 後置型でカタカナ文字列を伸ばす: 文字数指定
    execute 'lmap <silent> h' . i "\<C-G>u\<C-R>=tcvime#InputConvertKatakanaExtend(" . i . ")\<CR>"
    " 後置型交ぜ書き変換: 読みの文字数指定有り: 活用しない語
    execute 'lmap <silent> m' . i "\<C-G>u\<C-R>=tcvime#InputPostConvert(" . i . ",0)\<CR>"
    " 後置型交ぜ書き変換: 読みの文字数指定有り: 活用する語
    execute 'lmap <silent> n' . i "\<C-G>u\<C-R>=tcvime#InputPostConvert(" . i . ",1)\<CR>"
    " 後置型漢字→入力シーケンス変換(指定文字数)
    execute 'lmap <silent> ;' . i "\<C-G>u\<C-R>=tcvime#InputConvertKanji2Seq(" . i . ")\<CR>"
    " 後置型入力シーケンス→漢字変換(指定文字数)
    execute 'lmap <silent> z' . i "\<C-G>u\<C-R>=tcvime#InputConvertSeq2Kanji(" . i . ")\<CR>"
  endfor
  " 後置型でカタカナ文字列を伸ばす: カタカナより前でひらがなが続く間
  lmap <silent> h0 <C-G>u<C-R>=tcvime#InputConvertKatakanaExtend(0)<CR>
  " 後置型交ぜ書き変換: 読みの文字数指定無し: 活用しない語
  lmap <silent> m0 <C-G>u<C-R>=tcvime#InputPostConvertStart(0)<CR>
  " 後置型交ぜ書き変換: 読みの文字数指定無し: 活用する語
  lmap <silent> n0 <C-G>u<C-R>=tcvime#InputPostConvertStart(1)<CR>
  " 後置型入力シーケンス→漢字変換
  lmap <silent> z0 <C-G>u<C-R>=tcvime#InputConvertSeq2Kanji(0)<CR>
  " 後置型漢字→入力シーケンス変換(現位置からスペースまで)
  lmap <silent> ;0 <C-G>u<C-R>=tcvime#InputConvertKanji2Seq(0)<CR>
  " 後置型漢字→入力シーケンス変換(現位置からInsert mode開始位置または行頭まで)
  lmap <silent> ;9 <C-G>u<C-R>=tcvime#InputConvertKanji2SeqAll()<CR><Plug>TcvimeIDisableKeymap
  " tc2同様の後置型交ぜ書き変換を行うための設定:
  " " 活用しない語
  " lmap <silent> 18 <C-G>u<C-R>=tcvime#InputPostConvert(1, 0)<CR>
  " lmap <silent> 28 <C-G>u<C-R>=tcvime#InputPostConvert(2, 0)<CR>
  " lmap <silent> 38 <C-G>u<C-R>=tcvime#InputPostConvert(3, 0)<CR>
  " lmap <silent> 48 <C-G>u<C-R>=tcvime#InputPostConvert(4, 0)<CR>
  " " 活用する語(ただしtc2と違って、読みの文字数には活用語尾は含まない)
  " lmap <silent> 29 <C-G>u<C-R>=tcvime#InputPostConvert(2, 1)<CR>
  " lmap <silent> 39 <C-G>u<C-R>=tcvime#InputPostConvert(3, 1)<CR>
  " lmap <silent> 49 <C-G>u<C-R>=tcvime#InputPostConvert(4, 1)<CR>
  " lmap <silent> 59 <C-G>u<C-R>=tcvime#InputPostConvert(5, 1)<CR>
endfunction
endif
" tcvime }}}
" unite {{{
" The prefix key.
nnoremap    [unite]   <Nop>
nmap <unique> <Leader>f [unite]

" unite.vim keymap
" https://github.com/alwei/dotfiles/blob/3760650625663f3b08f24bc75762ec843ca7e112/.vimrc
nnoremap [unite]u  :<C-u>Unite -no-split<Space>
nnoremap <silent> [unite]f :<C-u>Unite<Space>buffer<CR>
nnoremap <silent> [unite]b :<C-u>Unite<Space>bookmark<CR>
nnoremap <silent> [unite]m :<C-u>Unite<Space>file_mru<CR>
nnoremap <silent> [unite]r :<C-u>UniteWithBufferDir file<CR>
nnoremap <silent> [unite]vr :UniteResume<CR>

" unite-build map
"nnoremap <silent> ,vb :Unite build<CR>
"nnoremap <silent> ,vcb :Unite build:!<CR>
"nnoremap <silent> ,vch :UniteBuildClearHighlight<CR>
" unite }}}
" vinarise {{{
let g:vinarise_enable_auto_detect = 0
let g:vinarise_detect_large_file_size = 0
" vinarise }}}
" {{{ vim-operator-surround
" operator mappings
map <silent>sa <Plug>(operator-surround-append)
map <silent>sd <Plug>(operator-surround-delete)
map <silent>sr <Plug>(operator-surround-replace)

let g:operator#surround#blocks = {
    \ 'markdown' : [
\       { 'block' : ["```\n", "\n```"], 'motionwise' : ['line'], 'keys' : ['`'] },
    \ ] }
" }}} vim-operator-surround

" vim-clang {{{
" set clang options for vim-clang
let g:clang_c_options = '-std=c11'
let g:clang_cpp_options = '-std=c++1z -stdlib=libc++ --pedantic-errors'
" let g:clang_cpp_options = g:clang_cpp_options . ' ' . system('pkg-config libxml++-2.6 --cflags')
let g:clang_diagsopt = 'rightbelow:6'
" vim-clang }}}
" bundle {{{
" syntax off
" set nocompatible
" filetype plugin indent off

if has('vim_starting')
  if &compatible
    set nocompatible
  endif
  set runtimepath+=~/.vim/bundle/neobundle.vim/
endif

call neobundle#begin(expand('~/.vim/bundle'))


" Package
NeoBundle 'Shougo/neobundle.vim'

" {{{ Operator/TextObjects
NeoBundle 'kana/vim-operator-user'
NeoBundle 'kana/vim-textobj-user'

NeoBundle 'rhysd/vim-operator-surround'
" NeoBundle 'tpope/vim-surround'

" Operator/TextObjects: LaTeX
NeoBundle 'rbonvall/vim-textobj-latex'
" }}} Operator/TextObjects

" Colorscheme
NeoBundle 'CSApprox'

" Markdown
NeoBundle 'hallison/vim-markdown'
NeoBundle 'vim-pandoc/vim-pandoc'
NeoBundle 'vim-pandoc/vim-pandoc-syntax'

" Modeline
NeoBundle 'Modeliner'

" Binary Edigor
NeoBundle 'Shougo/vinarise'

" Complation
NeoBundle 'Shougo/neocomplcache'

" Libraries
NeoBundle 'Shougo/unite.vim'
NeoBundle 'Shougo/vimproc'

" Filer
NeoBundle 'Shougo/vimfiler'

" Shell
NeoBundle 'Shougo/vimshell'

" File
" NeoBundle 'ctrlp.vim'

" ???
NeoBundle 'ujihisa/unite-locate'
" NeoBundle 'violetyk/cake.vim'
NeoBundle 'taglist.vim'
NeoBundle 'ref.vim'
NeoBundle 'The-NERD-tree'
NeoBundle 'The-NERD-Commenter'

" VCS: Git
NeoBundle 'fugitive.vim'
NeoBundle 'AndrewRadev/gapply.vim'


NeoBundle 'thinca/vim-quickrun'
NeoBundle 'thinca/vim-localrc'

" C/C++
NeoBundle 'justmao945/vim-clang'

" DB
" NeoBundle 'dbext.vim'

" HTML
NeoBundle 'ZenCoding.vim'

" Web
if version > 703
  NeoBundle 'TwitVim'
endif

" NeoBundle 'rails.vim'
NeoBundle 'Gist.vim'
NeoBundle 'motemen/hatena-vim'
" NeoBundle 'mattn/webapi-vim'
" NeoBundle 'mattn/unite-advent_calendar'
NeoBundle 'open-browser.vim'

NeoBundle 'jelera/vim-javascript-syntax'

" Text Editing
NeoBundle 'hsitz/VimOrganizer'
NeoBundle 'fuenor/qfixhowm'
NeoBundle 'fuenor/qfixgrep'
NeoBundle 'osyo-manga/unite-qfixhowm'

" NeoBundle 'VimOutliner' " It introduces smart paste, although it is not smart in Haskell source
NeoBundle 'VOoM'
NeoBundle 'WOIM.vim'

" CSV
NeoBundle 'Align'
NeoBundle 'csv.vim'

" Buffer
NeoBundle 'NrrwRgn'

" APL
NeoBundle 'ngn/vim-apl'

" Haskell
NeoBundle 'dag/vim2hs'
NeoBundle 'eagletmt/ghcmod-vim'
NeoBundle 'pbrisbin/html-template-syntax'
NeoBundle 'eagletmt/neco-ghc'
NeoBundle 'eagletmt/unite-haddock'

" JavaScript
NeoBundle 'marijnh/tern_for_vim'

" Color Scheme
NeoBundle 'altercation/vim-colors-solarized'

" Language {{{
" Greek
NeoBundle 'na4zagin3/pgreek.vim'
NeoBundle 'polytonic.utf-8.spl'

" Japanese
"NeoBundle 'tyru/skk.vim'
NeoBundle 'deton/tcvime'
" Language }}}

call neobundle#end()

filetype plugin indent on
syntax on

let g:neocomplcache_enable_at_startup = 1
" bundle }}}
" VimOrganizer {{{
let g:org_command_for_emacsclient ='emacsclient'
let g:org_agenda_select_dirs=["~/org"]
augroup MyVimOrganizer
  autocmd!
  autocmd BufNewFile,BufRead *.org setfiletype org
  autocmd FileType org :noremap <silent> <buffer> <localleader>a* :call OrgRunAgenda(strftime("%Y-%m-%d"),'w','')<cr>
  autocmd FileType org :noremap <silent> <buffer> <localleader>aa :call OrgRunAgenda(strftime("%Y-%m-%d"),'w','+ANY_TODO')<cr>
  autocmd FileType org :noremap <silent> <buffer> <localleader>at :call OrgRunAgenda(strftime("%Y-%m-%d"),'w','+UNFINISHED_TODOS')<cr>
  autocmd FileType org :noremap <silent> <buffer> <localleader>ad :call OrgRunAgenda(strftime("%Y-%m-%d"),'w','+FINISHED_TODOS')<cr>
augroup END
" VimOrganizer }}}
" XML {{{
let g:xml_syntax_folding = 1
augroup MyXML
  autocmd!
  autocmd BufNewFile,BufRead *.html :set foldmethod=syntax
augroup END
" XML }}}
" NeoComplcache {{{
if has('mac') && has('xim')
  let g:neocomplcache_auto_completion_start_length = 3
endif
" NeoComplcache }}}
" {{{ config
set background=dark
colorscheme koehler
set mouse=a
" }}} config
" XML {{{
let g:xml_syntax_folding = 1
" XML }}}
" Copyright (C) 2007 KaoriYa/MURAOKA Taro
