3

S	0	_	_	->	S	0 >	_ ^	_ ^
S	1	_	_	->	S	1 >	_ ^	_ ^
S	|	_	_	->	S	| >	_ ^	_ ^
S	_	_	_	->	S2	| <	_ ^	_ ^

S2	0	_	_	->	S2	0 <	_ ^	_ ^
S2	1	_	_	->	S2	1 <	_ ^	_ ^
S2	|	_	_	->	S2	| <	_ ^	_ ^
S2	_	_	_	->	GO	! >	_ ^	_ ^

S1	0	_	_	->	S1	0 <	_ ^	_ ^
S1	1	_	_	->	S1	1 <	_ ^	_ ^
S1	|	_	_	->	S1	| <	_ ^	_ ^
S1	!	_	_	->	GO	! >	_ ^	_ ^

GO	#0	_	_	->	CHECK1	#0 ^	_ ^	_ ^
CHECK1	#0	_	_	->	CHECK1	#0 >	_ ^	_ ^
CHECK1	|	_	_	->	CHECK2	| >	_ ^	_ ^
CHECK2	#0	_	_	->	CHECK3	#0 <	_ ^	_ ^

CHECK2	_	_	_	->	FINISH1	_ <	_ ^	_ ^

FINISH1	|	_	_	->	FINISH	_ <	_ ^	_ ^
FINISH	#0	_	_	->	FINISH	#0 <	_ ^	_ ^
FINISH	|	_	_	->	FINISH	| <	_ ^	_ ^
FINISH	!	_	_	->	AC	_ >	_ ^	_ ^

CHECK3	|	_	_	->	CHECK4	| <	_ ^	_ ^
CHECK4	#0	_	_	->	CHECK4	#0 <	_ ^	_ ^
CHECK4	|	_	_	->	CMP	| >	_ ^	_ ^
CHECK4	!	_	_	->	CMP	! >	_ ^	_ ^

CMP	#0	_	_	->	CMP	_ >	#0 >	_ ^
CMP	|	_	_	->	CMP2	_ >	_ ^	_ ^
CMP2	#0	_	_	->	CMP2	_ >	_ ^	#0 >
CMP2	|	_	_	->	CMPT	_ <	_ ^	_ ^
CMPT	_	_	_	->	CMPT	_ <	_ ^	_ ^
CMPT	|	_	_	->	CMP3	| >	_ <	_ <
CMPT	!	_	_	->	CMP3	! >	_ <	_ <

CMP3	_	#0	#1	->	CMP3	_ ^	#0 <	#1 <
CMP3	_	_	#0	->	LT	_ ^	_ ^	#0 <
LT	_	_	#0	->	LT	_ ^	_ ^	#0 <
CMP3	_	#0	_	->	GT	_ ^	#0 <	_ ^
GT	_	#0	_	->	GT	_ ^	#0 <	_ ^
CMP3	_	_	_	->	CMP4	_ ^	_ >	_ >
CMP4	_	#0	#0	->	CMP4	_ ^	#0 >	#0 >
CMP4	_	0	1	->	JLT	_ ^	0 <	1 <
CMP4	_	_	_	->	JLT	_ ^	_ <	_ <
CMP4	_	1	0	->	JGT	_ ^	1 <	0 <

JLT	_	#0	#0	->	JLT	_ ^	#0 <	#0 <
JLT	_	_	_	->	LT	_ ^	_ ^	_ ^
JGT	_	#0	#0	->	JGT	_ ^	#0 <	#0 <
JGT	_	_	_	->	GT	_ ^	_ ^	_ ^

LT	_	_	_	->	LT1	_ ^	_ >	_ ^
LT1	_	#0	_	->	LT1	#0 >	_ >	_ ^
LT1	_	_	_	->	LT2	| >	_ ^	_ >
LT2	_	_	#0	->	LT2	#0 >	_ ^	_ >
LT2	_	_	_	->	BACK	| <	_ ^	_ ^

BACK	#0	_	_	->	BACK	#0 <	_ ^	_ ^
BACK	|	_	_	->	GO	| >	_ ^	_ ^

GT	_	_	_	->	GT2	_ ^	_ ^	_ >
GT2	_	_	#0	->	GT2	#0 >	_ ^	_ >
GT2	_	_	_	->	GT1	| >	_ >	_ ^
GT1	_	#0	_	->	GT1	#0 >	_ >	_ ^
GT1	_	_	_	->	S1	| <	_ ^	_ ^
