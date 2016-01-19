# /etc/skel/.bash_profile

# This file is sourced by bash for login shells.  The following line
# runs your .bashrc and is recommended by the bash info pages.
[[ -f ~/.bashrc ]] && . ~/.bashrc

if [[ "$(hostname)" = "desk-pc" ]] ; then
  export QSYS_ROOTDIR="/home/mrty/altera_lite/15.1/quartus/sopc_builder/bin"
  export ALTERAOCLSDKROOT="/home/mrty/altera_lite/15.1/hld"
fi
