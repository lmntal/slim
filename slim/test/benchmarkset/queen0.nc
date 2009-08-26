never { /* !([]!q) */
T0_init:
	if
	:: (1) -> goto T0_init
	:: (q) -> goto accept_all
	fi;
accept_all:
	skip
}

