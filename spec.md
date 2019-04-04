# Arithmetic Term Logic Grammar
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


# Logical Truths
1. Any statement of the form "all X are X" (-X+X) is always true.
2. The negation of a logical falsehood is always true.

# Logical Falsehoods
1. Any statement of the form "some X is not X" (+X-X) is always false.
2. The negation of a logical truth is always false.

# Unary Statement Operations
1. Conversion
Logical quantity and quality remain the same. Subject and predicate are swapped.
+/-A+/-B -> +/-B+/-A
Equivalent for E and I statements.

2. Contraposition
The result of negating the subject and predicate of a statement's converse.
+/-A+/-B -> +/-(-B)+/-(-A)
Equivalent for A and O statements.

3. Predicate Obversion
Logical quality is flipped and the predicate is negated.
+/-A+/-B -> +/-A-/+(-B)
Equivalent for all statements.

4. Categorical Contradiction
Logical quality and quality are flipped.
+/-A+/-B -> -/+A-/+B
Not equivalent for any statements.

5. Statement Obversion
The negation of a statement's categorical contradictory.
+/-A+/-B -> -(-/+A-/+B)
Equivalent for all statements.

6. Direct Contradiction
The negation of a statement.
+/-A+/-B -> -(+/-A+/-B)
Not equivalent for any statements.

7. Double Negation
The negation of the negation of a statement.
+/-A+/-B -> -(-(+/-A+/-B))
Equivalent for all statements.

# Unary Statement Operations with Relational Terms
1. Relational Converse

# Binary Statement Operations
1. Dictum de Omni

# M-ary Statement Operations
1. Reductio ad Absurdum

2. Principle of Syllogistic Validity
