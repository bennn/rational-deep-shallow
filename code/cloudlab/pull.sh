for X in `seq 0 114` ; do
  echo ${X};
  ADR=$( head -n $(( ${X} + 1 )) qt | tail -n 1 )
  scp ${ADR}:rds-cloudlab/output/1/0-run-info.rkt/17-quadT.out 2022-09-27/${X}.out
done
