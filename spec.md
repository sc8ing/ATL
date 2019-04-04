# Grammar
statement ::=
	| (statement)
	| -(statement)
	| +upperterm+upperterm
	| +upperterm+upperterm
	| -upperterm+upperterm
	| -upperterm-upperterm

upperterm ::=
	| upperterm indexes
	| upperterm*
	| uppercasechar
	| (innerterm)

naturalnumber ::= 1 | 2 | 3 | ... | infinity

indexes ::=
	| e
	| naturalnumber
	| naturalnumber, indexes

uppercasechar ::= A | ... | Z

innerterm ::=
	| upperterm
	| -upperterm
	| compoundterm

compoundterm ::=
	| +upperterm+upperterm
	| +upperterm+upperterm
	| -upperterm+upperterm
	| -upperterm-upperterm
