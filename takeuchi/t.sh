#!/bin/bash

t () {
	local x=$(eval $1)
	local y=$(eval $2)

	if ((x <= y)); then
		echo $y
	else
		local z=$(eval $3)
		t "t 'echo $((x-1))' 'echo $y' 'echo $z'" \
			"t 'echo $((y-1))' 'echo $z' 'echo $x'" \
			"t 'echo $((z-1))' 'echo $x' 'echo $y'"
	fi
}

if (($# != 3)); then
	echo "You need to specify three integers."
	exit 1
fi

t "echo $1" "echo $2" "echo $3"
