for sh in ${HOME}/.profile.d/*.sh ; do
        [ -r "$sh" ] && . "$sh"
done

# vim: set ft=sh :

export QSYS_ROOTDIR="/home/mrty/altera_lite/15.1/quartus/sopc_builder/bin"

export ALTERAOCLSDKROOT="/home/mrty/altera_lite/15.1/hld"
