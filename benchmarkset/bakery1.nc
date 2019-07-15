never { /* !([](wait -> <>crit)) */
T0_init:
	if
	:: (1) -> goto T0_init
	:: (!crit && wait) -> goto accept_S2
	fi;
accept_S2:
	if
	:: (!crit) -> goto accept_S2
	fi;
}

