never { /* !(<>p -> <>q) */
T0_init:
	if
	:: (!q) -> goto T0_init
	:: (p && !q) -> goto accept_S2
	fi;
accept_S2:
	if
	:: (!q) -> goto accept_S2
	fi;
}

