#! /usr/bin/env bash

now() {
    printf $(command date "+%s")
}

starttime=$(now)
emacsloc=$(dirname $(dirname $(which emacs)))
configure_opts="--prefix $emacsloc"
make_opts=
quit_char="'G'"
emacs="ftp://ftp.gnu.org/pub/gnu/emacs/emacs-21.3.tar.gz"
file=${emacs##*/}      # emacs-21.3.tar.gz
dir=${file%.tar.gz}    # emacs-21.3
temp=/tmp/$$-$RANDOM
log=~/emacs.log

[ -r $log ] && {
  rm -f $log
}
touch $log

mkdir $temp
cd $temp

wget emacs >>$log 2>&1
printf "** Downloaded ${emacs}\n** Unpacking ${file}\n" >> $log
gzip -cd $file | tar xvf -  >>$log 2>&1

cd $dir

printf "** Replacing quit_char to ${quit_char}\n" >> $log
# Need to be more careful here.
sed "s/^\(  quit_char = \)\(Ctl ('g')\|'G'\)\(;\)/\1$quit_char\3/" \
    src/keyboard.c                                                 \
    > src/keyboard.c

printf "** Configuring emacs\n"
./configure $configure_opts >>$log 2>&1
printf "** Making emacs\n"
make $make_opts >>$log 2>&1

# Just in case.
cp -R $emacsloc ..
rm -rf $emacsloc
make install >>$log 2>&1

if [ "$?" != "0" ]; then
    printf "** Unable to install new emacs\n"
    printf "** Reverting to old one\n"
    rm -rf $emacsloc
    cp -R ../emacs $emacsloc >>$log 2>&1
else
    secs=$(($(now)-starttime))
    mins=$(($secs/60))
    printf "Done installing emacs; (secs: %f, mins: %f)\n" >>$log
fi
    
cd
rm -rf $temp
