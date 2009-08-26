never { /* !(([]<>f1 -> []<>f2) -> ([](ncs -> <>cs))) */
T0_init:
	if
	:: (1) -> goto T0_S1
	:: (!f1) -> goto T0_S6
	:: (!cs && ncs) -> goto T1_S7
	:: (!cs && ncs && !f1) -> goto accept_S8
	:: (1) -> goto T0_S2
	:: (!cs && ncs) -> goto T2_S5
	fi;
T0_S1:
	if
	:: (1) -> goto T0_S1
	:: (!f1) -> goto T0_S6
	:: (!cs && ncs) -> goto T1_S7
	:: (!cs && ncs && !f1) -> goto accept_S8
	fi;
T0_S6:
	if
	:: (!f1) -> goto T0_S6
	:: (!cs && ncs && !f1) -> goto accept_S8
	fi;
T1_S7:
	if
	:: (!cs) -> goto T1_S7
	:: (!cs && !f1) -> goto accept_S8
	fi;
accept_S8:
	if
	:: (!cs && !f1) -> goto accept_S8
	fi;
T0_S2:
	if
	:: (1) -> goto T0_S2
	:: (!cs && ncs) -> goto T2_S5
	fi;
T2_S5:
	if
	:: (!cs) -> goto T2_S5
	:: (!cs && f2) -> goto accept_S5
	fi;
accept_S5:
	if
	:: (!cs) -> goto T2_S5
	:: (!cs && f2) -> goto accept_S5
	fi;
}

