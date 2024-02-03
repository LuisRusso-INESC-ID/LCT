#!/usr/bin/bash

function testF {
    echo -n "Testing $1 $2 "

    ./tester "$1" "$2" | tee -p splayInput pointerInput > /dev/null &
#    ./linTester "$1" "$2" | tee -p splayInput pointerInput > /dev/null &

    cat splayInput | ./CLI > splayOutput &
#    cat splayInput | clisp lct.lisp | tail -n +11 > splayOutput &

    cat pointerInput | ./pCLI > pointerOutput &
#    cat pointerInput | python LCT.py > pointerOutput &
#    cat pointerInput | java -cp . -Djava.library.path=. jLCT > pointerOutput &

    diff -q splayOutput pointerOutput

    if [ "$?" -eq 0 ]
    then
	echo "success"
    else
	echo "failure"
	endS
	exit 1
    fi
}

function endS {
    rm splayOutput
    rm pointerOutput

    rm splayInput
    rm pointerInput
}

#set -x

if [ -p splayInput ]
then
    rm splayInput
fi
mkfifo splayInput

if [ -p pointerInput ]
then
    rm pointerInput
fi
mkfifo pointerInput

if [ -p splayOutput ]
then
    rm splayOutput
fi
mkfifo splayOutput

if [ -p pointerOutput ]
then
    rm pointerOutput
fi
mkfifo pointerOutput

for N in 10 100 1000 10000 100000 1000000 # 10000000
#for N in 10 100 1000 10000
do
    for O in 10 100 1000 10000 100000 1000000 # 10000000
#    for O in 10 100 1000 10000
    do
	testF "${N}" "${O}"
    done
done

endS

#set +x

exit 0
