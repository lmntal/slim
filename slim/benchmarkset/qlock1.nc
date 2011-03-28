never { /* !([](ncs -> <>cs)) */
T0_init:
	if
	:: (!cs && ncs) -> goto accept_S2
	:: (1) -> goto T0_init
	fi;
accept_S2:
	if
	:: (!cs) -> goto accept_S2
	fi;
}

