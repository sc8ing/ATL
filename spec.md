# Grammar
statement ::=
	| (statement)
	| -(statement)
	| +upperterm+upperterm
	| +upperterm+upperterm
	| -upperterm+upperterm
	| -upperterm-upperterm

upperterm ::=
	| upperterm*
	| uppercasechar
	| (innerterm)

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
