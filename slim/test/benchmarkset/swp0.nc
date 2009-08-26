never { /* ![]( s -> <>a ) */
T0_init:
	if
	:: (!a && s) -> goto accept_S2
	:: (1) -> goto T0_init
	fi;
accept_S2:
	if
	:: (!a) -> goto accept_S2
	fi;
}

