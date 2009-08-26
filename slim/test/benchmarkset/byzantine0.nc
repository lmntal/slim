never { /* []<>(!a && !b)) */
T0_init:
	if
	:: (!a && !b) -> goto accept_S1
	:: (1) -> goto T0_init
	fi;
accept_S1:
	if
	:: (!a && !b) -> goto accept_S1
	:: (1) -> goto T0_init
	fi;
}
