never { /* !((([]<>f1 -> []<>f2) && ([]<>f3 -> []<>f4)) -> ([](p -> <>q))) */
T0_init:
	if
	:: (1) -> goto T0_S1
	:: (!q && p) -> goto T2_S32
	:: (1) -> goto T0_S2
	:: (!f3) -> goto T0_S21
	:: (!q && p) -> goto T2_S24
	:: (!q && p && !f3) -> goto T2_S25
	:: (1) -> goto T0_S3
	:: (!f1) -> goto T0_S14
	:: (!q && p) -> goto T1_S16
	:: (!q && p && !f1) -> goto T4_S18
	:: (1) -> goto T0_S4
	:: (!f3) -> goto T0_S5
	:: (!f1) -> goto T0_S6
	:: (!f1 && !f3) -> goto T0_S7
	:: (!q && p) -> goto T1_S8
	:: (!q && p && !f3) -> goto T1_S9
	:: (!q && p && !f1) -> goto T3_S10
	:: (!q && p && !f1 && !f3) -> goto accept_S11
	fi;
T0_S1:
	if
	:: (1) -> goto T0_S1
	:: (!q && p) -> goto T2_S32
	fi;
T2_S32:
	if
	:: (!q) -> goto T2_S32
	:: (!q && f2) -> goto T4_S32
	:: (!q && f2 && f4) -> goto accept_S32
	fi;
T4_S32:
	if
	:: (!q) -> goto T4_S32
	:: (!q && f4) -> goto accept_S32
	fi;
accept_S32:
	if
	:: (!q) -> goto T2_S32
	:: (!q && f2) -> goto T4_S32
	:: (!q && f2 && f4) -> goto accept_S32
	fi;
T0_S2:
	if
	:: (1) -> goto T0_S2
	:: (!f3) -> goto T0_S21
	:: (!q && p) -> goto T2_S24
	:: (!q && p && !f3) -> goto T2_S25
	fi;
T0_S21:
	if
	:: (!f3) -> goto T0_S21
	:: (!q && p && !f3) -> goto T2_S25
	fi;
T2_S24:
	if
	:: (!q) -> goto T2_S24
	:: (!q && !f3) -> goto T2_S25
	fi;
T2_S25:
	if
	:: (!q && !f3) -> goto T2_S25
	:: (!q && f2 && !f3) -> goto accept_S25
	fi;
accept_S25:
	if
	:: (!q && !f3) -> goto T2_S25
	:: (!q && f2 && !f3) -> goto accept_S25
	fi;
T0_S3:
	if
	:: (1) -> goto T0_S3
	:: (!f1) -> goto T0_S14
	:: (!q && p) -> goto T1_S16
	:: (!q && p && !f1) -> goto T4_S18
	fi;
T0_S14:
	if
	:: (!f1) -> goto T0_S14
	:: (!q && p && !f1) -> goto T4_S18
	fi;
T1_S16:
	if
	:: (!q) -> goto T1_S16
	:: (!q && !f1) -> goto T4_S18
	fi;
T4_S18:
	if
	:: (!q && !f1) -> goto T4_S18
	:: (!q && !f1 && f4) -> goto accept_S18
	fi;
accept_S18:
	if
	:: (!q && !f1) -> goto T4_S18
	:: (!q && !f1 && f4) -> goto accept_S18
	fi;
T0_S4:
	if
	:: (1) -> goto T0_S4
	:: (!f3) -> goto T0_S5
	:: (!f1) -> goto T0_S6
	:: (!f1 && !f3) -> goto T0_S7
	:: (!q && p) -> goto T1_S8
	:: (!q && p && !f3) -> goto T1_S9
	:: (!q && p && !f1) -> goto T3_S10
	:: (!q && p && !f1 && !f3) -> goto accept_S11
	fi;
T0_S5:
	if
	:: (!f3) -> goto T0_S5
	:: (!f1 && !f3) -> goto T0_S7
	:: (!q && p && !f3) -> goto T1_S9
	:: (!q && p && !f1 && !f3) -> goto accept_S11
	fi;
T0_S6:
	if
	:: (!f1) -> goto T0_S6
	:: (!f1 && !f3) -> goto T0_S7
	:: (!q && p && !f1) -> goto T3_S10
	:: (!q && p && !f1 && !f3) -> goto accept_S11
	fi;
T0_S7:
	if
	:: (!f1 && !f3) -> goto T0_S7
	:: (!q && p && !f1 && !f3) -> goto accept_S11
	fi;
T1_S8:
	if
	:: (!q) -> goto T1_S8
	:: (!q && !f3) -> goto T1_S9
	:: (!q && !f1) -> goto T3_S10
	:: (!q && !f1 && !f3) -> goto accept_S11
	fi;
T1_S9:
	if
	:: (!q && !f3) -> goto T1_S9
	:: (!q && !f1 && !f3) -> goto accept_S11
	fi;
T3_S10:
	if
	:: (!q && !f1) -> goto T3_S10
	:: (!q && !f1 && !f3) -> goto accept_S11
	fi;
accept_S11:
	if
	:: (!q && !f1 && !f3) -> goto accept_S11
	fi;
}

