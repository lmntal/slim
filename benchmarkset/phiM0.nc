never { /* !([](h -> <>e)) */
T0_init:
	if
	:: (1) -> goto T0_init
	:: (!e && h) -> goto accept_S2
	fi;
accept_S2:
	if
	:: (!e) -> goto accept_S2
	fi;
}

