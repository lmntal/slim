never { /* !((([]<>f1 -> []<>f2) && ([]<>f3 -> []<>f4)) -> (<>p)) */
T0_init:
	if
	:: (!p && f2 && f4) -> goto accept_S1
	:: (!p && f2) -> goto T3_S1
	:: (!p) -> goto T1_S1
	:: (!p) -> goto T1_S2
	:: (!p && !f3) -> goto T1_S13
	:: (!p) -> goto T0_S3
	:: (!p && !f1) -> goto T3_S10
	:: (!p) -> goto T0_S4
	:: (!p && !f3) -> goto T0_S5
	:: (!p && !f1) -> goto T2_S6
	:: (!p && !f1 && !f3) -> goto accept_S7
	fi;
accept_S1:
	if
	:: (!p && f2 && f4) -> goto accept_S1
	:: (!p && f2) -> goto T3_S1
	:: (!p) -> goto T1_S1
	fi;
T3_S1:
	if
	:: (!p && f4) -> goto accept_S1
	:: (!p) -> goto T3_S1
	fi;
T1_S1:
	if
	:: (!p && f2 && f4) -> goto accept_S1
	:: (!p && f2) -> goto T3_S1
	:: (!p) -> goto T1_S1
	fi;
T1_S2:
	if
	:: (!p) -> goto T1_S2
	:: (!p && !f3) -> goto T1_S13
	fi;
T1_S13:
	if
	:: (!p && !f3) -> goto T1_S13
	:: (!p && f2 && !f3) -> goto accept_S13
	fi;
accept_S13:
	if
	:: (!p && !f3) -> goto T1_S13
	:: (!p && f2 && !f3) -> goto accept_S13
	fi;
T0_S3:
	if
	:: (!p) -> goto T0_S3
	:: (!p && !f1) -> goto T3_S10
	fi;
T3_S10:
	if
	:: (!p && !f1) -> goto T3_S10
	:: (!p && !f1 && f4) -> goto accept_S10
	fi;
accept_S10:
	if
	:: (!p && !f1) -> goto T3_S10
	:: (!p && !f1 && f4) -> goto accept_S10
	fi;
T0_S4:
	if
	:: (!p) -> goto T0_S4
	:: (!p && !f3) -> goto T0_S5
	:: (!p && !f1) -> goto T2_S6
	:: (!p && !f1 && !f3) -> goto accept_S7
	fi;
T0_S5:
	if
	:: (!p && !f3) -> goto T0_S5
	:: (!p && !f1 && !f3) -> goto accept_S7
	fi;
T2_S6:
	if
	:: (!p && !f1) -> goto T2_S6
	:: (!p && !f1 && !f3) -> goto accept_S7
	fi;
accept_S7:
	if
	:: (!p && !f1 && !f3) -> goto accept_S7
	fi;
}

