NR == 1 {
	print "{" $0 "}."
}

NR == 2 {
	print "{@garbage_rulesets__. " $0 "}/"
}

NR == 3 && $0 ~ /ok/ {
	print " :- ok."
}

NR == 3 && $0 ~ /ng/ {
	print ", ok :- ng. ok."
}
