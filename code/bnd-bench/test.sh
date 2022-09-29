RACO=/home/ben/code/blame-trail/rds/rds-cloudlab/racket-8.6.0.1/bin/raco
RACKET=/home/ben/code/blame-trail/rds/rds-cloudlab/racket-8.6.0.1/bin/racket

cd $1
echo "begin" `pwd`
rm -rf cfg
mkdir cfg

echo "= typed"
cp both/*rkt typed/*rkt cfg/.
cd cfg && ${RACO} make -v main.rkt && ${RACKET} main.rkt && cd ..
for U in untyped/*rkt ; do
  echo "= typed / ${U}"
  cp typed/*rkt cfg/. ; cp ${U} cfg/. ;
  cd cfg && ${RACO} make -v main.rkt && ${RACKET} main.rkt && cd ..
done

echo "= shallow"
cp both/*.rkt shallow/*rkt cfg/.
cd cfg && ${RACO} make -v main.rkt && ${RACKET} main.rkt && cd ..
for U in untyped/*rkt ; do
  echo "= shallow / ${U}"
  cp shallow/*rkt cfg/. ; cp ${U} cfg/. ;
  cd cfg && ${RACO} make -v main.rkt && ${RACKET} main.rkt && cd ..
done

