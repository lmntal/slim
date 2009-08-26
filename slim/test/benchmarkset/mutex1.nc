never { /* !([](r -> <>c)) */
T0_init:
	if
	:: (!c && r) -> goto accept_S2
	:: (1) -> goto T0_init
	fi;
accept_S2:
	if
	:: (!c) -> goto accept_S2
	fi;
}

