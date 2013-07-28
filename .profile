for sh in ${HOME}/.profile.d/*.sh ; do
        [ -r "$sh" ] && . "$sh"
done

# vim: set ft=sh :
