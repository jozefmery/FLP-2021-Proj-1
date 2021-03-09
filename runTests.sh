#!/usr/bin/env bash

# File:         runTests.sh
# Author:       Jozef Méry - xmeryj00@vutbr.cz
# Project:      FLP-2021-xmeryj00-simplify-bkg
# Date:         8.3.2021
# Description:  Automated test script.

# Console colors
RED="\033[0;31m"
GREEN="\033[0;32m"
YEL="\033[0;33m"
CLEAR="\033[0m"

testPass=0
testFiles=10
((testTotal=$testFiles * 3)) # 3 tests per file
bin="./simplify-bkg"
testDir="./test"

print() {
  # interpret backslashes
  echo -e $1
}

fail() {

  print "[${RED}FAIL${CLEAR}] $1"
}

pass() {

  print "[${GREEN}PASS${CLEAR}] $1"
}

exitPass() {

  print "[${GREEN}TEST SET PASSED${CLEAR}]"
  exit 0
}

exitFail() {

  print "[${RED}TEST SET FAILED${CLEAR}]"
  exit 1
}

callBin() {

  eval "$bin $1"
}

formatTestPath() {

  local path="$testDir/test$1"
  echo "$path"
}

runTest() {
  # $1 - flag "i", "1", or "2"
  # $2 - test number

  # echo $1
  # echo $2

  local path=$(formatTestPath $2)
  callBin "-$1 $path.in" > $path.temp
  
  diff $path.temp $path.out${1}
  
  if [ "$?" == "0" ]
  then
    ((++testPass))
    pass "[TEST $2] $bin -$1 $path.in"
  else
    fail "[TEST $2] $bin -$1 $path.in"
  fi

  rm $path.temp
}

testFlags() {

  local flags=("i" "1" "2")
  for f in "${flags[@]}"
  do 
    runTest $f $1 # $1 - test number
  done 
}

runTests() {

  for (( i=1; i<=$testFiles; ++i ))
  do
    testFlags $i
  done
}

# clearly mark program entry point and exit
main() {

  print "Running ${bin} tests ..."

  if [[ -f ${bin} ]]
  then
    pass "${bin} found"
  else
    fail "${bin} file not found."
    exitFail
  fi
  print "-------------------------------------------------"
  runTests
  print "-------------------------------------------------"
  print "TESTS FINISHED"
  print "-------------------------------------------------"
  print "${testPass} PASSED OUT OF ${testTotal}"

  if [[ $testPass == $testTotal ]]
  then
    exitPass
  else
    exitFail
  fi
}

main