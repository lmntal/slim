never { /* !(!<>(p && !q && !r)) */
T0_init:
	if
	:: (1) -> goto T0_init
	:: (!q && !r && p) -> goto accept_all
	fi;
accept_all:
	skip
}

