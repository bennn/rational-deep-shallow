for X in typed/*rkt ; do
 cp untyped/*rkt cfg/. ; cp ${X} cfg/. ; cd cfg ; echo ""; echo ${X} ; raco make main.rkt && racket main.rkt ; cd .. ;
 cp shallow/$(basename ${X}) cfg/. ; cd cfg ; echo ""; echo "shallow/$(basename ${X})" ; raco make main.rkt && racket main.rkt ; cd .. ;
done
