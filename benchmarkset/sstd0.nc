never { /* !(!(<>(p && (q || r || s)))) */
T0_init:
	if
	:: (1) -> goto T0_init
	:: (p && r) || (p && s) || (p && q) -> goto accept_all
	fi;
accept_all:
	skip
}

