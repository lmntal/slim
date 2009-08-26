never { /* !(([]<>f1 -> []<>f2) -> ([](r -> <>c))) */
T0_init:
	if
	:: (1) -> goto T0_S1
	:: (!f1) -> goto T0_S6
	:: (!c && r) -> goto T1_S7
	:: (!c && r && !f1) -> goto accept_S8
	:: (1) -> goto T0_S2
	:: (!c && r) -> goto T2_S5
	fi;
T0_S1:
	if
	:: (1) -> goto T0_S1
	:: (!f1) -> goto T0_S6
	:: (!c && r) -> goto T1_S7
	:: (!c && r && !f1) -> goto accept_S8
	fi;
T0_S6:
	if
	:: (!f1) -> goto T0_S6
	:: (!c && r && !f1) -> goto accept_S8
	fi;
T1_S7:
	if
	:: (!c) -> goto T1_S7
	:: (!c && !f1) -> goto accept_S8
	fi;
accept_S8:
	if
	:: (!c && !f1) -> goto accept_S8
	fi;
T0_S2:
	if
	:: (1) -> goto T0_S2
	:: (!c && r) -> goto T2_S5
	fi;
T2_S5:
	if
	:: (!c) -> goto T2_S5
	:: (!c && f2) -> goto accept_S5
	fi;
accept_S5:
	if
	:: (!c) -> goto T2_S5
	:: (!c && f2) -> goto accept_S5
	fi;
}

