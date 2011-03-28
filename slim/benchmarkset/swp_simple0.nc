never { /* !([](send -> <>ack)) */
T0_init:
	if
	:: (!ack && send) -> goto accept_S2
	:: (1) -> goto T0_init
	fi;
accept_S2:
	if
	:: (!ack) -> goto accept_S2
	fi;
}

