echo "DONE"
wc -l output/1/0*/*out
echo "TODO"
wc -l output/1/0*/*in
echo ""
for X in output/1/0*/*.in ; do
  PRE=${X%.*}
  echo "$(basename ${PRE})"
  wc -l ${PRE}.out
  wc -l ${X}
done
