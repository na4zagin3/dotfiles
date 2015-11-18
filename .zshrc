# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=100000
SAVEHIST=100000
bindkey -v
setopt autocd extendedglob
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall

[ -f /etc/zsh/git-flow-completion.zsh ] && source /etc/zsh/git-flow-completion.zsh

zstyle ':completion:*' completer _expand _complete _ignored _correct _approximate
zstyle ':completion:*' file-sort name
zstyle ':completion:*' matcher-list '' 'm:{[:lower:]}={[:upper:]}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
zstyle :compinstall filename "${HOME}/.zshrc"

autoload -Uz compinit
compinit
# End of lines added by compinstall
setopt auto_cd auto_pushd list_packed prompt_subst append_history auto_list auto_menu
setopt auto_param_keys auto_param_slash hist_ignore_dups hist_ignore_all_dups hist_verify
setopt magic_equal_subst mark_dirs print_eight_bit share_history PROMPT_PERCENT PROMPT_SUBST PROMPT_BANG
setopt auto_menu auto_cd correct auto_name_dirs auto_remove_slash
setopt extended_history inc_append_history share_history hist_ignore_dups hist_ignore_space prompt_subst
setopt pushd_ignore_dups rm_star_silent sun_keyboard_hack
setopt extended_glob list_types no_beep always_last_prompt
setopt cdable_vars sh_word_split auto_param_keys

function chpwd() { ls }
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}

[ -f ~/.zshprompt ] && source ~/.zshprompt
[ -f ~/.zshalias ] && source ~/.zshalias
[ -f ~/.zshtest ] && source ~/.zshtest
[ -f ~/.zshvar ] && source ~/.zshvar
[ -f ~/.zshkey ] && source ~/.zshkey
[[ "$(hostname)" =~ ".*\.cad\.ce\.titech\.ac\.jp" ]] && export USERNAME="$USER"

# vim: ft=zsh :
