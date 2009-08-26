never { /* !([](p -> <>q)) */
T0_init:
	if
	:: (!q && p) -> goto accept_S2
	:: (1) -> goto T0_init
	fi;
accept_S2:
	if
	:: (!q) -> goto accept_S2
	fi;
}

