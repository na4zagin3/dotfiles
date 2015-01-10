" vim:set ts=8 sts=2 sw=2 tw=0 fdm=marker: (この行に関しては:help modelineを参照)
"
" An example for a Japanese version vimrc file.
" 日本語版のデフォルト設定ファイル(vimrc) - Vim7用試作
"
" Last Change: 10-Jan-2015.
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
  autocmd BufNewFile,BufRead *.tup :setfiletype tup
  autocmd BufNewFile,BufRead Tupfile :setfiletype tup
  autocmd BufReadCmd   *.epub      call zip#Browse(expand("<amatch>"))
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
  let $PLUGINROOT=$HOME.'/vim-plugin'
  let qfixmemo_chenv_dir = '~/qfixmemo'
  let qfixmemo_dir = qfixmemo_chenv_dir

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

" enviroment }}}

" howm {{{
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
"  au BufWritePost *.hs :GhcModCheckAndLintAsync
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
" unite {{{
" The prefix key.
"nnoremap    [unite]   <Nop>
"nmap    <Leader>f [unite]

" unite.vim keymap
" https://github.com/alwei/dotfiles/blob/3760650625663f3b08f24bc75762ec843ca7e112/.vimrc
"nnoremap [unite]u  :<C-u>Unite -no-split<Space>
"nnoremap <silent> [unite]f :<C-u>Unite<Space>buffer<CR>
"nnoremap <silent> [unite]b :<C-u>Unite<Space>bookmark<CR>
"nnoremap <silent> [unite]m :<C-u>Unite<Space>file_mru<CR>
"nnoremap <silent> [unite]r :<C-u>UniteWithBufferDir file<CR>
"nnoremap <silent> ,vr :UniteResume<CR>

" vinarise
let g:vinarise_enable_auto_detect = 1

" unite-build map
"nnoremap <silent> ,vb :Unite build<CR>
"nnoremap <silent> ,vcb :Unite build:!<CR>
"nnoremap <silent> ,vch :UniteBuildClearHighlight<CR>
" unite }}}
" bundle {{{
syntax off
set nocompatible
filetype plugin indent off

if has('vim_starting')
  set nocompatible
  set runtimepath+=~/.vim/bundle/neobundle.vim/
endif

call neobundle#begin(expand('~/.vim/bundle'))

NeoBundle 'CSApprox'
NeoBundle 'hallison/vim-markdown'
NeoBundle 'vim-pandoc/vim-pandoc'
NeoBundle 'Modeliner'

NeoBundle 'Shougo/vinarise'
NeoBundle 'Shougo/neobundle.vim'
NeoBundle 'Shougo/neocomplcache'
NeoBundle 'Shougo/unite.vim'
NeoBundle 'Shougo/vimproc'
NeoBundle 'Shougo/vimfiler'
NeoBundle 'Shougo/vimshell'
NeoBundle 'Shougo/neobundle.vim'
NeoBundle 'ujihisa/unite-locate'
" NeoBundle 'violetyk/cake.vim'
NeoBundle 'tpope/vim-surround'
NeoBundle 'taglist.vim'
NeoBundle 'ZenCoding.vim'
NeoBundle 'ref.vim'
NeoBundle 'The-NERD-tree'
NeoBundle 'The-NERD-Commenter'
NeoBundle 'fugitive.vim'
NeoBundle 'TwitVim'
NeoBundle 'thinca/vim-quickrun'
NeoBundle 'thinca/vim-localrc'
NeoBundle 'dbext.vim'
" NeoBundle 'rails.vim'
NeoBundle 'Gist.vim'
NeoBundle 'motemen/hatena-vim'
NeoBundle 'mattn/webapi-vim'
NeoBundle 'mattn/unite-advent_calendar'
NeoBundle 'open-browser.vim'
NeoBundle 'ctrlp.vim'
NeoBundle 'jelera/vim-javascript-syntax'

" Text Editing
NeoBundle 'hsitz/VimOrganizer'
NeoBundle 'fuenor/qfixhowm'
NeoBundle 'fuenor/qfixgrep'
NeoBundle 'osyo-manga/unite-qfixhowm'

NeoBundle 'VimOutliner'
NeoBundle 'VOoM'
NeoBundle 'WOIM.vim'

" CSV
NeoBundle 'Align'

" Buffer
NeoBundle 'NrrwRgn'

" APL
NeoBundle 'ngn/vim-apl'

" Haskell
NeoBundle 'dag/vim2hs'
NeoBundle 'eagletmt/ghcmod-vim'
NeoBundle 'pbrisbin/html-template-syntax'
NeoBundle 'ujihisa/neco-ghc'
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
" Copyright (C) 2007 KaoriYa/MURAOKA Taro
