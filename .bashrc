# /etc/skel/.bashrc
#
# This file is sourced by all *interactive* bash shells on startup,
# including some apparently interactive shells such as scp and rcp
# that can't tolerate any output.  So make sure this doesn't display
# anything or bad things will happen !


# Test for an interactive shell.  There is no need to set anything
# past this point for scp and rcp, and it's important to refrain from
# outputting anything in those cases.
if [[ $- != *i* ]] ; then
	# Shell is non-interactive.  Be done now!
	return
fi


# Put your fun stuff here.

# Check for an interactive session
#[ -z "$PS1" ] && return

alias ls='ls --color=auto'
#PS1='[\u@\h \W]\$ '
# export SCIPY_PIL_IMAGE_VIEWER=display

if [[ "$(hostname)" = "desk-pc" ]] ; then
  export QSYS_ROOTDIR="/home/mrty/altera_lite/15.1/quartus/sopc_builder/bin"
  export ALTERAOCLSDKROOT="/home/mrty/altera_lite/15.1/hld"
fi
