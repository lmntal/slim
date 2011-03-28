never { /* !(([]<>f1 -> []<>f2) -> (<>p -> <>q)) */
T0_init:
	if
	:: (!q) -> goto T0_S1
	:: (p && !q) -> goto T2_S8
	:: (!q) -> goto T0_S2
	:: (!q && !f1) -> goto T0_S3
	:: (p && !q) -> goto T1_S4
	:: (p && !q && !f1) -> goto accept_S5
	fi;
T0_S1:
	if
	:: (!q) -> goto T0_S1
	:: (p && !q) -> goto T2_S8
	fi;
T2_S8:
	if
	:: (!q) -> goto T2_S8
	:: (!q && f2) -> goto accept_S8
	fi;
accept_S8:
	if
	:: (!q) -> goto T2_S8
	:: (!q && f2) -> goto accept_S8
	fi;
T0_S2:
	if
	:: (!q) -> goto T0_S2
	:: (!q && !f1) -> goto T0_S3
	:: (p && !q) -> goto T1_S4
	:: (p && !q && !f1) -> goto accept_S5
	fi;
T0_S3:
	if
	:: (!q && !f1) -> goto T0_S3
	:: (p && !q && !f1) -> goto accept_S5
	fi;
T1_S4:
	if
	:: (!q) -> goto T1_S4
	:: (!q && !f1) -> goto accept_S5
	fi;
accept_S5:
	if
	:: (!q && !f1) -> goto accept_S5
	fi;
}

