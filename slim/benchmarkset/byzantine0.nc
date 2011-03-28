never { /* !([]<>(a || b)) */
T0_init:
	if
	:: (1) -> goto T0_init
	:: (!a && !b) -> goto accept_S2
	fi;
accept_S2:
	if
	:: (!a && !b) -> goto accept_S2
	fi;
}

