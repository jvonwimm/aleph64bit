#!/bin/sh
cat > comptest.F <<EOF
      program comptest
      a = 0
      end
EOF
GCCVSN=`cpp -dM comptest.F | grep __VERSION__ | cut -d " " -f3 | cut -c 2`
FC=" "
[ "$GCCVSN" = "3" ]&&FC=g77
[ "$GCCVSN" -ge "4" ]&&FC=gfortran
[ "$GCCVSN" -ge "5" ]&&FC=`echo ${FC} -no-pie`
if [ "$GCCVSN" = " " ]; then
  echo " "
  echo "====================================="
  echo "Expected GCC compiler suite not found"
  echo "====================================="
  rm -f comptest.*
  exit 1
else
  check="Checking for Fortran Compiler... "
  $FC -c comptest.F >/dev/null 2>&1
  if test -s comptest.o; then
    echo "${check}${FC}"
  else
    echo "${check} no"
    rm -f comptest.*
    exit 1
  fi
fi
rm -f comptest.*
echo $FC > compversion
