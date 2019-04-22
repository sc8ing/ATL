# Arithmetic Term Logic Grammar

statement ::=
	| (statement)
	| -(statement)
	| +upperterm+upperterm
	| +upperterm+upperterm
	| -upperterm+upperterm
	| -upperterm-upperterm

upperterm ::=
	| uppercasechar indexes
	| upperterm* indexes
	| (innerterm) indexes
	| relationalterm indexes

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

relationalterm ::=
	| uppercasechar indexes
	| (-uppercasechar) indexes
	| (relationalterm+upperterm)
	| (relationalterm-upperterm)

# ATL Semantics

The following, while not a complete explanation, should be enough for most people to get started and use this simple proof-checker.

## Statements

A formal argument in ATL is composed of a series of premises expressed as ATL statements followed by a conclusion statement prefixed with a slash. Each statement is written on its own line. ATL supports four types of statements, shown in the diagram below.

| 						| Affirmative | Negative |
| ----------- | ----------- | -------- |
| **Universal** | -T+T				| -T-T		 |
| **Particular** | +T+T				| +T-T		 |

The letter "T" above denotes a term in ATL, which is discussed below. The quality of a statement can be either affirmative or negative, the quantity universal or particular. Each unique combination of quantity and quality is referred to as a category. These types are referred to by letters.

| 						| Affirmative | Negative |
| ----------- | ----------- | -------- |
| **Universal** | A   				| E   		 |
| **Particular** | I   				| O   		 |

Below are some examples in English corresponding to the four types of categorical statments ATL supports.

| 						| Affirmative | Negative |
| ----------- | ----------- | -------- |
| **Universal** | All cows eat grass | All cows don't eat grass |
| **Particular** | Some cows eat grass | Some cows don't eat grass |

## Terms

There are three types of terms in ATL: simple, compound, and relational.

### Simple Terms

Simple terms are the building blocks the other terms rely on and are written with a single uppercase letter. If we were to write the above English examples in ATL, we could denote "cows" with "C" and "eater of grass" with "G", resulting in the following:

|                | Affirmative | Negative |
| -------------- | ----------- | -------- |
| **Universal**  | -C+G        | -C-G     |
| **Particular** | +C+G        | +C-G     |

### Compound terms

Compound terms are combinations of other terms, which can be nested. The following table shows ATL's compound term notation as it relates to general set notation:

| ATL  | Set Equivalent |
| ---- | -------------- |
| +T+T | T $\cap$ T     |
| +T-T | T $\cap$ T^c^  |
| -T+T | T^c^$\cup$T    |
| -T-T | T^c^$\cup$T^c^ |
| --T--T   | T$\cup$T |

The last line is added for convenience and is equivalent to -(-T)-(-T). This notation allows us to express terms like "brown cow" as "+B+C", where "B" denotes all things that are brown.

### Relational Terms

Relational terms are terms that take other terms as their arguments. For example, "owner" is a relational term in the statement "Some farmer is an owner of a cow" (or, similarly, "owns" in "Some farmer owns a cow"). As far as concerns the validity or invalidity of an argument, the order of arguments to a relational term is irrelevant; the above statements are logically equivalent to "Some cow is owned by a farmer". It's therefore more powerful to express the two ideas "to own" and "to be owned by" as the same relational term. For now, let the term "Ox,y" mean "x owns y".

For reasons concerning potential ambiguities with this notation that won't be discussed here, all arguments to a relational term are assigned a unique number and the relational term itself postfied with these numbers in order of their application separated by commas. For example, let "Bx,y,z" mean "x borrows y from z", "A" mean Ashley, "G" a piece of gum, and "C" Charlie. Then the following statements in the table below are all logically equivalent (though some are awkward in English):

| ATL Notation | English Equivalent                     |
| ------------ | -------------------------------------- |
| +A1+((B123+G2)+C3) | Ashley borrows gum from Charlie        |
| +A1+((B123+C2)+G3) | Ashley is lended, by Charlie, some gum |
| +G1+((B123+C2)+A3) | Gum is borrowed from Charlie by Ashley |
| +G1+((B123+A2)+C3) | Gum is borrowed by Ashley from Charlie |
| +C1+((B123+A2)+G3) | Charlie lends Ashley gum               |
| +C1+((B123+G2)+C3) | Charlie lends gum to Ashley            |

Note that the numbers and their order are arbitrary; we just as easily could have used 5, 6, and 2.

### Negation

Any term may be negated by prefacing it with a minus sign to denote its complement. For example, the statement "+C-G" is logically equivalent to "+C+(-G)". In English, "Some cow doesn't eat grass" means the same thing as "Some cow eats only non-grass". Indeed, this correlation holds for all statements whatsoever and is a rule discussed below, here referred to as predicate obversion.

# Summary of Logical Rules

Below are all of the logical rules relied upon to manipulate arguments in this program.

## Logical Truths
1. Any statement of the form "all X are X" (-X+X) is always true.
2. The negation of a logical falsehood is always true.

## Logical Falsehoods
1. Any statement of the form "some X is not X" (+X-X) is always false.
2. The negation of a logical truth is always false.

## Unary Statement Operations
1. **Conversion**
  Logical quantity and quality remain the same. Subject and predicate are swapped.
  +/-A+/-B $\rightarrow$ +/-B+/-A
  Equivalent for E and I statements.
  Applicable to all statements.

  

2. **Contraposition**
  The result of negating the subject and predicate of a statement's converse.
  +/-A+/-B $\rightarrow$ +/-(-B)+/-(-A)
  Equivalent for A and O statements.
  Applicable to all statements.

  

3. **Predicate Obversion**
  Logical quality is flipped and the predicate is negated.
  +/-A+/-B $\rightarrow$ +/-A-/+(-B)
  Equivalent for all statements.
  Applicable to all statements.

  

4. **Categorical Contradiction**
  Logical quality and quality are flipped.
  +/-A+/-B $\rightarrow$ -/+A-/+B
  Not equivalent for any statements.
  Applicable to all statements.

  

5. **Statement Obversion**
  The negation of a statement's categorical contradictory.
  +/-A+/-B $\rightarrow$ -(-/+A-/+B)
  Equivalent for all statements.
  Applicable to all statements.

  

6. **Direct Contradiction**
  The negation of a statement.
  +/-A+/-B $\rightarrow$ -(+/-A+/-B)
  Not equivalent for any statements.
  Applicable to all statements.

  

7. **Double Negation**
  The negation of the negation of a statement.
  +/-A+/-B $\rightarrow$ -(-(+/-A+/-B))
  Equivalent for all statements.
  Applicable to all statements.

  

8. **Term Double Negation**
   The double negation of any term in a statement.
   +/-A+/-B $\rightarrow$ +/-(-(-A))+/-B and +/-A+/-(-(-B))
   Equivalent for all terms.
   Applicable to all terms.

   

9. **Term Conversion**
   Reversing the order of terms in a compound term.
   +/-(+A+B)+/-C $\rightarrow$ +/-(+B+A)+/-C
   Equivalent for ++ and -- terms.
   Applicable to all terms.

   

10. **Term Obversion**
  The negation of each term in a compound terms converse.
  +/-(+A+B)+/-C $\rightarrow$ +/-(+(-B)+(-A))+/-C
  Equivalent for -+ and +- terms.
  Applicable to all terms.

  

11. **De Morgan's**
    The application of De Morgan's law to a compound term.
    +/-(-A-B)+/-C $\rightarrow​$ +/-(-(+A+B))+/-C
    Equivalent for all terms.
    Applicable to all terms.

12. **Association**

11. **Detatchment**

12. **Relational Converse**

## Binary Statement Operations
1. **Dictum de Omni**

   

## M-ary Statement Operations
1. **Reductio ad Absurdum**

   

2. **Principle of Syllogistic Validity**
