# scala_buchberger

## About

Based on "Ideals, Varieties, and Algorithms (D. Cox, et. al.),
I wrote a simple scala programs for Buchberger algorithm
and supporting library for computer algebra system.

Sample codes are shown in scala worksheet `QWorksheet.sc`, 
which can be played in Eclipse with scala worksheet plugin.
The unit-test codes are also useful to understand usage of the library. 

This library is aimed for solving the exercises of the textbook
rather than practical use.

## Library

* package basic
   + package.scala : Defines implicit conversions from string.
   + Q.scala : Defines trait `Field` and its implementation of `Q`, rational number.
   + MonomialOrder.scala : Defines trait `MonomialOrder` and its implementations `LexOrder`, `GrLexOrder`, `GRevLexOrder`.
   + Mono.scala : Defines `Monomial`.
   + Term.scala : Defines trait `Term` and `QTerm`, its implementation on `Q`.
   + Poly.scala : Defines trait `Poly` and `QPoly`, its implementation on `Q`.
   + QWorksheet.sc : scala worksheet for demonstrations.
   
## Conversions from String

The library defines some implicit conversions from `String` to `Mono`, `QTerm`, `QPoly`.
Conversions require monomial order object as `implicit val` or explicit argument. 

````
"<polynomial>".poly : conversion to QPoly
"<term>".term : conversion to QTerm
"<monomial>".mono : conversion to Mono
"<rational>".q : conversion to Q
````

Those BNFs are as follows:

````
<polynomial> ::= <term> | <term> " + " <polynomial> | <term> " - " <polynomial>.

<term> ::= 
  <monomial>                ;; coefficient = 1
| "-" <monomial"            ;; coefficient = -1
| <integer> <monomial>      ;; coefficient = integer
| <rational> <monomial>     ;; coefficient = rational
| <integer>                 ;; monomial = 1
| <rational>.               ;; monomial = 1

<monomial> ::= 
  <single> 
| <single> <monomial>.

<single> ::=
  <letter>                  ;; degree=1
| <letter> "^" <integer>.

<letter> ::= "a" | ... | "z" | "A" | ... | "Z".

<integer> ::= <digits> | "-" <digits>.
<rational> ::= <integer> "/" <digits>.
<digits> ::= <digit> | <digit> <digits>.
<digit> ::= "0" | "1" | ... | "9".
````

In `polynomial`, each terms are separated with "+" or "-" with left and right spaces 
for convenience of parsing.
The `letter` must be defined in `MonomialOrder` object.

## To do

* Refactoring in more functional-way (avoid use of `var`, `while`, and arrays).  
* Transformation of Term/MonomialOrder to homogenize input of Buchberger.
* Optimization with suger.
* Not only `Q` but also calcurations on F_p.
