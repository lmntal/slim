never { /* !(<>p -> <>q) */
T0_init:
	if
	:: (p && !q) -> goto accept_S2
	:: (!q) -> goto T0_init
	fi;
accept_S2:
	if
	:: (!q) -> goto accept_S2
	fi;
}

