![](fifth.png)

## An (extremely brief, pre-alpha) overview of Fifth. #

Fifth is a programming language. It began as a thought experiment
to design a language faster than C, adopting RISC-V as a sole target mainly
for the benefits of eschewing portability. It gradually became a
"RISC-V swiss army knife", mired in architecture-specific behavior
that has now been relegated entirely to the inline assembly extension.
At present, it's still a long way from being implemented,
but I think it's emerged as a modern, simple, and powerful language
for embedded and high-performance disciplines.

A formal standard is forthcoming, but progress has been frustratingly slow.
In the meantime, this document offers a brief outline of the language,
with an emphasis on its novel features and improvements over C.
Until then, syntactic and semantic ambiguities might exist; expect
novel features to disappear in the future.

Like the standard, this document uses the lowering notation `abc ⟷ xyz`
to indicate that two portions of code `abc` and `xyz` are interchangable.

Some terminology:
* In Fifth, "static" means "at compile time". Static expressions must be
evaluated at compile time, static variables must have compile-time-known values, etc.
Unlike C, stasis has nothing to do with storage duration or symbol visibility.
* A malformed portion of code may "creak"
(cause the compiler to emit a warning and continue compilation)
or "croak"
(cause the compiler to emit an error and halt).

## Basics

Fifth source code is UTF-8 encoded text.
Conventionally, it's stored in files with the extension `.5th`.

Comments begin with an `#` outside of a character or string literal 
and continue until the end of the line.
```
 # This is a comment.
```

## Types

Fifth is statically and (somewhat) strongly typed.

`lit` is the type of literals.
`lit` operations are performed at arbitrary precision;
however, `lit` is an implicitly static type, so all are resolved at
compile time. `lit`s cast implicitly to any other type.

`nat` and `int` are unsigned and two's complement signed integers, respectively. 
Their widths are equal to `XLEN`,
i.e. the number of bits in an address on the target machine.
Many of their behaviors -- overflow, division by zero, etc. -- are undefined,
at least when using the typical operators.
Fixed-width variants -- `nat8`, `int64`, etc. -- are also available.

`bool` is a boolean type of unspecified width defined only for the values `0` and `1`.
`bool`s are used in conditional expressions,
and are usually obtained from comparison operators.
`true` and `false` are reserved words of type `bool` 
with values `1` and `0`, respectively.

Later, we'll see a few other types, including pointers.
Floating point types and operations are defined in a separate extension.

## Numeric Literals

Numeric literals represent fixed, immediate numeric values.
Radix prefixes allow writing in binary, octal, and decimal;
single underscores are allowed between successive digits.
The exponent suffixes `e` (unless in a hexadecimal literal) and `p`
allow scaling by a power of 10 or 2, respectively.
```
5
0.2
4_000_000
0b01101001
0o70.34
0xDEAD_BEEF
6.18e-9
0x1.FFp0b10
1p5
```
Note that all literals are of type `lit`; no distinction is made between
"integer" and "floating point" literals.

## Character Literals

Unicode code points are represented by enclosing characters in single quotes.
Backslashes create typical escape sequences.
```
'V'
'⅕'
'Ä'
'\n'
'\0'
'\x3C'
'\u2306'
'\U000110F5'
'\\'
```
Character literals containing multiple
characters are equivalent to lists of each character.
```
'aeiou' ⟷ 'a', 'e', 'i', 'o', 'u'
'\r\n\0' ⟷ 0x0D, 0x0A, 0
```

## Lists

Lists are comma-separated sets of one or more values.
```
3, 4, 5
-2, -2.1, -2.2, 'Q'
0xB6
```
Lists are static; their size, and all of their elements, must be
known at compile time.

## String Literals #

String literals are a shorthand for allocating immutable UTF-8-encoded character strings.
They consist of one or more characters enclosed in double quotes.
Unlike C, strings are *not* implicitly null-terminated.
```
"Here's some text."
"Hello, world!\0"
"\"höwdÿ\""
```
Strings evaluate to a list containing a `nat8 ptr` and a `lit`, the latter being the size of the string in bytes.
Strings separated by whitespace catenate.

## Ranges

Ranges are a useful shorthand for writing lists.
`a .. b` denotes a list from `a` to (and including) `b`.
`a ... b` denotes a list from `a` to (and excluding) `b`.
`a .. i .. b` and `a ... i ... b` denote lists
from `a` in steps of `i` to `b` inclusive or exclusive, respectively.
```
0..5 ⟷ 0, 1, 2, 3, 4, 5
0...5 ⟷ 0, 1, 2, 3, 4
5..0 ⟷ 5, 4, 3, 2, 1, 0
a...( a + 4 ) ⟷ a, a + 1, a + 2, a + 3
0..1..5 ⟷ 0..5 ⟷ 0, 1, 2, 3, 4, 5
5..-1..0 ⟷ 5..0 ⟷ 5, 4, 3, 2, 1, 0
3..2..9.5 ⟷ 3, 5, 7, 9
6...-0.5...4 ⟷ 6, 5.5, 5, 4.5
```
A range croaks if
* `a`, `b`, or `i` are nonstatic
* `i` is zero
* `i` > 0 but `a` < `b`, or vice versa

## Operators
Values are combined by operators to form expressions.

<table>
<thead>
<tr>
<th style="text-align:center">Symbol</th>
<th>Operator</th>
<th>Precedence</th>
<th>Associativity</th>
</tr>
</thead>
<tbody>
<tr><td style="text-align:center"><code>(</code> <code>)</code>, <code>[</code> <code>]</code>, <code>.</code></td><td>of</td><td>1</td><td>left-to-right</td></tr>
<tr><td style="text-align:center"><code>+</code></td><td>positive</td><td>2</td><td>right-to-left</td></tr>
<tr><td style="text-align:center"><code>-</code></td><td>negative</td><td>2</td><td>right-to-left</td></tr>
<tr><td style="text-align:center"><code>!</code></td><td>not</td><td>2</td><td>right-to-left</td></tr>
<tr><td style="text-align:center"><code>⁎</code></td><td>mutiplication</td><td>3</td><td>left-to-right</td></tr>
<tr><td style="text-align:center"><code>/</code></td><td>division</td><td>3</td><td>left-to-right</td></tr>
<tr><td style="text-align:center"><code>%</code></td><td>remainder</td><td>3</td><td>left-to-right</td></tr>
<tr><td style="text-align:center"><code>&amp;</code></td><td>and</td><td>3</td><td>left-to-right</td></tr>
<tr><td style="text-align:center"><code>&amp;!</code></td><td>and not</td><td>3</td><td>left-to-right</td></tr>
<tr><td style="text-align:center"><code>&lt;&lt;</code></td><td>left shift</td><td>3</td><td>left-to-right</td></tr>
<tr><td style="text-align:center"><code>&gt;&gt;</code></td><td>right shift</td><td>3</td><td>left-to-right</td></tr>
<tr><td style="text-align:center"><code>+</code></td><td>addition</td><td>4</td><td>left-to-right</td></tr>
<tr><td style="text-align:center"><code>-</code></td><td>subtraction</td><td>4</td><td>left-to-right</td></tr>
<tr><td style="text-align:center"><code>|</code></td><td>or</td><td>4</td><td>left-to-right</td></tr>
<tr><td style="text-align:center"><code>|!</code></td><td>or not</td><td>4</td><td>left-to-right</td></tr>
<tr><td style="text-align:center"><code>^</code></td><td>xor</td><td>4</td><td>left-to-right</td></tr>
<tr><td style="text-align:center"><code>^!</code></td><td>xor not</td><td>4</td><td>left-to-right</td></tr>
<tr><td style="text-align:center"><code>==</code></td><td>equal</td><td>5</td><td>explicit</td></tr>
<tr><td style="text-align:center"><code>!=</code></td><td>not equal</td><td>5</td><td>explicit</td></tr>
<tr><td style="text-align:center"><code>&gt;</code></td><td>greater than</td><td>5</td><td>explicit</td></tr>
<tr><td style="text-align:center"><code>&lt;</code></td><td>less than</td><td>5</td><td>explicit</td></tr>
<tr><td style="text-align:center"><code>&gt;=</code></td><td>greater than or equal</td><td>5</td><td>explicit</td></tr>
<tr><td style="text-align:center"><code>&lt;=</code></td><td>less than or equal</td><td>5</td><td>explicit</td></tr>
<tr><td style="text-align:center"><code>in</code></td><td>in</td><td>5</td><td>explicit</td></tr>
<tr><td style="text-align:center"><code>!in</code></td><td>not in</td><td>5</td><td>explicit</td></tr>
<tr><td style="text-align:center"><code>not</code></td><td>longform not</td><td>6</td><td>right-to-left</td></tr>
<tr><td style="text-align:center"><code>or</code></td><td>longform or</td><td>7</td><td>left-to-right</td></tr>
<tr><td style="text-align:center"><code>and</code></td><td>longform and</td><td>8</td><td>left-to-right</td></tr>
</tbody>
</table>

Assigners -- `=`, `+=`, `++`, etc. -- are discussed later.

## The Of Operator

The `of` operator is used for a variety of purposes, including
function and macro calls, 
pointer dereferences,
element access,
and casting.
It has three broadly interchangable syntaxes:
```
( a ) ⟷ [ a ] ⟷ .a
```


## Arithmetic Operators

The `+`, `-`, `*`, `/`, and `%` operators compute
addition, subtraction, multiplication, division,
and remainder.
The unary `-` operator negates its operand.
```
2 + 3 ⟷ 5
8 - 1 ⟷ 7
-4 * -8 ⟷ 32
-9 / 3 ⟷ -3
9 % -2 ⟷ --1
```
 
## Bitwise Operators #
TODO: examples that don't depict `lit` semantics

The unary operator `!` inverts the bits of its operand.

The `&`, `|`, and `^` operators compute the bitwise
and, or and exclusive or of their operands, respectively.

The `&!`, `|!`, and `^!` operators are also provided
as a shorthand.
```
a &! b ⟷ a & !( b )
a |! b ⟷ a | !( b )
a ^! b ⟷ a ^ !( b )
```

`<<` performs a leftward logical bit shift.
`>>` performs a rightward arithmetic bit shift if its
first operand is of type `int`, or a logical shift otherwise.
Shifts of more than 8 * the size of the righthand operand are undefined.

Bitfields could be the subject of a future extension.

## Comparison Operators

The binary operators `==`, `!=`, `>`, `<`, `>=`, and `<=`
compare their operands and yield a result of type `bool`.
```
3 != 5 ⟷ true
3 > 5 ⟷ false
3 <= 5 ⟷ true
```
The operators `in` and `!in` assess whether the lefthand operand
is equal or nonequal to any member of the righthand list.
```
5 in 3, 4, 5 ⟷ true
5 !in 1...9 ⟷ false
5 in ⟷ false
```
When operating on boolean values, `&`, `|`, `^`, `!`, etc.
behave as logical operators.
```
true & false ⟷ false
true | false ⟷ true
true ^ false ⟷ true
!true ⟷ false
```
However, the binary operators have frustratingly high precidence,
and `!` can be hard to read in complex expressions.

More suitable are the longform operators `and`, `or`, and `not`.
They have low precidence and automatically cast their operands to `bool`.
`and` and `or` are short-circuiting -- their second operand
isn't evaluated if the first is sufficient to determine their truthiness.
```
true and 125 ⟷ true
0.0 or false ⟷ false
not not 0x4C ⟷ true
```

## Assignment Statements

Programs are formed from statements, of which assignment statements are the most fundamental.
Assignment statements consist of a list (the "left-hand side"), an assigner, 
and another list (the "right-hand side"). Statements are terminated with semicolons, but
Fifth will automatically insert semicolons at line breaks according to an as-of-yet unwritten
protocol.

`=` is the basic assigner.
```
v = 5
x, y, z = 2, 4, 6  ⟷ x = 2; y = 4; z = 6
a, b = b, a # swaps a and b
```
The number of elements on each side don't have to match.
Extra elements on the left are assigned the last element on the right.
Extra elements on the right are discarded.
`_` can be used to "skip" elements.
```
h, j, k = 1, 0 ⟷ h = 1; j = 0; k = 0
m, n = 3, 4, 5 ⟷ m = 3; n = 4
c, _, d = 13, 14, 15 ⟷ c = 13; d = 15
```
On the left-hand side, a type name followed by a
new identifier is a variable declaration.
Variables cannot be declared without an assignment,
but `_` can be used to leave their value undefined.
Note that *every* newly declared variable must have a type in front of it.
```
nat n = 16
int r, int s = _ ⟷ int r = _; int s = _
```
For left-hand-side lists without declarations,
operators of precedence 3 and 4 can be used in compound assignments.
```
k /= 2 ⟷ k = k / 2
p, q &= 0b1001, 0b0110 ⟷ p = p & 0b1001; q = q & 0b0110
```
`++` and `--` are assigners with no right-hand side,
equivalent to `+= 1` and `-= 1`, respectively.
```
c -- ⟷ c -= 1
a, b ++ ⟷ a, b += 1
```

## Casts

Fifth has two types of cast: prescriptive
and descriptive. Both are achieved with the `of` operator.

Prescriptive casts potentially modify values to achieve a numerically correct result.
```
int32( r )
(nat8 ptr)( y )
```
Descriptive casts interpret an existing value as being of a new type.
```
m.nat
k(int16)
```
Generally, static types (`lit`s, `type`s, `macro`s, etc.) cannot be descriptively cast.
Similarly, descriptively casting to a wider type invokes undefined behavior.

## Conditionals

The `if` keyword allows for the conditional execution of a statement or
block of statements based on the result of a boolean expression.
```
if x > 5: return false

if q != j + 6
{
	q = j + 6
	j -= 6
}
```
The `el` keyword presents a subsequent alternative if the `if` expression fails.
An omitted boolean expression is equivalent to `true`.
```
if r > 0:
	t += r
el r < 0:
	t += -r + 1
el:
	t, r = 4 - z
```
Fifth has no "switch" statement. 
However, using lists and the `in` operator is usually terse enough.
```
if c in '0'..'9': val = c - '0'
el c in 'ABCDEF': val = c - 'A' + 10
el c in 'abcdef': val = c - 'a' + 10
el: return false
```
A single assignment statement can be provided before the boolean expression, separated by a semicolon.
Any declared variables are visible in all branches of the conditional.
```
if nat j, nat k = readj(), readk(); j != k
{
	w = j + k
}
el
{
	w = j << 1
	return readk()
}
```
Finally, conditionals can be evaluated at compile time with the `static` qualifier.
Conditionals are allowed at global scope, where they are implicitly static.
```
static if 'A' !in env.extensions: useatomics = false
```

## Loops

`for` is Fifth's looping construct.

In its simplest form, `for` is an infinite loop.
```
for: update()
```
A single boolean expression can be provided, discontinuing the loop when it becomes false.
```
for u != v
{
	u, v = getu( u ), getv( v )
}
```
Additionally, a single assignment statement can be placed before the expression, separated by a semicolon.
The scope of any declared variables is limited to the loop.
```
for nat i = 0; i < 10
{
	i += xvalue( i )
}
```
Finally, an arbitrary number of assignment statements can be placed after the expression,
to be executed at the conclusion of each iteration.
```
for nat x, y = 0, 10; x + y < 16; x += 2; y --
{
	setsquare( x, y )
}
```
While these constructs are perfectly fine,
idiomatic loops will typically use "in" syntax,
achieved by replacing the boolean expression with a pseudo-statement
consisting of a type, identifier, `in`, and a list.
```
for nat i in 0...10: displayval( i )

for j, k = false; nat8 c in '{}()[]<>'; j, k = !j, !k
{
	j, k = putbracket( c, j, k )
}
```
If you want a loop without a body, explictly use a colon and a semicolon.
```
for ; n > 0; n --; k ++:;
```

One other looping construct exists: `til` differs only from `for` in that its first iteration
is performed unconditionally.
```
til nat32 x, nat32 r = _;  r - x > range; n ++
{
	x = rand32()
	r = x % range
}
```

## Labels and Jumps
`jump` is Fifth's primary control flow keyword.

A bare `jump` exits the current block.
```
for
{
	if not update(): jump
}
```

Labels are created with an identifier and a colon.
Any label within the current function or macro may be jumped to, regardless of scope.
```
if r < 0: jump error
for k != 0: k = nextmode( r )
error:
return false
```

`jump for` (or `jump til`, regardless of loop format) skips to the next loop iteration.
```
for nat8 i in 0...2...256
{
	if i & 1p6 != 0: jump for # skip when 6th bit is set
	feedbyte( i )
	setbyte( i )
}
```

## Functions #

Functions are expressed at global scope
as a list of return values, the keyword `func`,
an identifier, and a list of parameters,
followed by a definition.
```
nat func hypots( nat b, nat b )
{
	return a * a + b * b
}
```
Return values and parameters aren't necessary;
in fact, parentheses aren't either.
```
func qroutine
{
	if bank( 2 ) < bvalue: return
	avalue = 0xC4
	cvalue = bank( 1 )
}
```

Function arguments are mutable and passed by value. 
Lists of multiple values may be returned.
```
int, int func polarspiral( int t )
{
	return 1, 4
}
```
Return values may be named. Named return values exist implicitly within the function,
and may be omitted from a `return` statement. If all return values are named,
the function implicitly returns at the end.
Also, parameters and arguments may share names.
```
nat x func square nat x: x *= x

( nat8 C, bool isletter )func capitalize( nat8 c )
{
	if c in 'a'...'z': return c &! 0b00100000, true
	el c in 'A'...'Z': return c, true
	return c, false
}
```
The rightmost parameters and return values can be given static default values.
```
TODO
```
Functions can be used as first-class values.
The actual "value" of a function is roughly akin to that of a function pointer.
```
nat func nat routine = readlines
nat r = routine( 5 )
```

## Macros

TODO: macros
TLDR: macros are pass-by-value and evaluated statically,
enabling things like overloading without mangling or polluting the calling convention.

## Type Definitions

New types can be declared like variables.
```
type byte = nat8
type Q16_16 = int32
```
Types don't have to be scalar. Aggregates are lists of types,
of which individual elements can be accessed via indexes or optional names.
```
type pair = bool, bool
type vec3 = int x, int y, int z

pair p = true, false
bool b = p.0

vec3 v = 3, 6, 9
v.x = 12
```

## Defer

TODO: is defer even necessary?

## Pointers

Pointers refer to locations in memory.

The `of` operator -- conventionally, `[` `]` -- is used to dereference pointers.
An empty dereference is equivalent to a dereference with an offset of zero.
An `of` with multiple elements expands distributively into a list of dereferences.
```
p[] ⟷ p[ 0 ] ⟷ ( p + 0 )[]

p[ 1..3 ] ⟷ p[ 1, 2, 3 ] ⟷ p[ 1 ], p[ 2 ], p[ 3 ]
```
Combining pointers and lists distributively allows for the terse expression of
memory manipulation routines. (Naturally, it's up to the implementation
to identify and vectorize them.)
```
# Zero a region of memory, i.e. memset( p, 0, 256 * sizeof( p ) ).
p[ 0...256 ] = 0

# Copy a region of memory, i.e. memmove( u, v, 0x7FF * sizeof( u ) ).
# Asserting the two regions are nonoverlapping could speed things up.
u[ 0..0x07FF ] = v[ 0..0x07FF ]

# Decrement every fourth element in a region of memory.
s[ 0..4..0x2FF ] --
```
Pointer arithmetic is performed as a multiple of alignment --
e.g. adding 1 to a `nat32 ptr` effectively increments the underlying
address by 4. If breaking this alignment is required, pointers
are granular down to increments of the inverse of their size.
```
nat32 ptr p = 1/2 ⟷ nat32 ptr p = (nat32 ptr)( 0x00000002 )
p += 0.25 # advance by one byte
```
Unlike C, Fifth has no "null pointers" -- the zero-valued
pointer is no different from any other.
Multiple returns are the preferred means of communicating that a
pointer is invalid.

Also unlike C, Fifth has no "address of" operator;
variables, etc. are nonaddressable.
Obtaining something to point to requires allocation.

## Allocation

Allocation is performed with `make`, assigned to a pointer of determinable type.
```
nat ptr a = make[ 5 ] 1, 2, 3, 4, 5
```
`make` is invoked with one or both of the `of` operator and a succeeding list.
The former contains one nonzero, possibly nonstatic `nat` specifying the number of
objects to allocate. The latter specifies the values of the objects.
If the former is missing or empty, the list determines the size of the allocation.
If the latter is missing, the values of the objects are undefined.
```
nat ptr a = make 1, 2, 3  ⟷ 
nat ptr a = make[] 1, 2, 3  ⟷ 
nat ptr a = make[ 3 ] 1, 2, 3  ⟷ 
nat ptr a = make[ 3 ]
a[ 0...3 ] = 1, 2, 3
```
Aggregates can be also be allocated. 
```
type triple = nat, nat, nat

triple ptr a = make[ 2 ] 1, 1, 1, 2, 2, 2
```
For optimization purposes, allocated values
may be padded, aligned, or reordered.
Qualifying a type or `make` invocation 
with the `<packed>` attribute prevents this.
```
<packed> type entry = 
nat32 value,
nat8 id,
nat8 # padding

nat32 ptr b = <packed> make[ 64 ]
```
After the end of its containing function, using memory allocated with `make`
invokes undefined behavior.
Beyond this, Fifth doesn't specify *how* it is allocated (statically,
on the stack, on the heap, etc.).

`make` can be qualified with `static`.
`static make` has the additional restriction that its allocation size
must be known statically, but allows using its memory at any subsequent
point in the program.
At global scope, `make` is implicitly static.

TODO: possible name candidates: `make`, `mem`, `alloc`, `array`, `get`, `loc`, etc.

## Unions

Values that need to be descriptively cast often might benefit from
being declared with multiple types. Such a value has the width of its
widest type, but semantics according to the leftmost type unless descriptively cast.
```
nat or int b = 5

int8 or int16 or int32 b = 257

( nat8 or int16 ) ptr p = make[] 1, 2, 3, 4 
```
TODO: resolve lots of pitfalls!

## env

TODO: `env` has a lot of fields

## Customs

By default, functions and global variables have internal linkage --
that is, they're invisible outside of the current file.
To make a symbol openly visible, use `export`.
```
export lit v = 5
export nat64 r = 0x70000000

export nat func square( nat x ): return x * x
```
Likewise, to use a symbol declared in another compilation unit,
use `import`. Generally, no values are assigned to these imports
(although an imported function *may* have a body containing assertions or static conditionals).
```
import int32 q
import lit w

import any ptr calloc( nat num, nat size );
import any ptr memchr( <const> any ptr ptr_, int32 value, nat num );
```

## Inclusion

`import` has another function: including files.
```
import "hashtable.5th"
import "include/linear.5th" "include/trig.5th"
```
Fifth prizes inline libraries ("header-only libraries" to C programmers)
for their simplicity and optimizability.
(Naturally, linkable libraries will also provide "definition" files requiring importing.)

`import` statements may include an identifier,
placing all identifiers in those files into the given namespace.
```
import "linearalgebra.5th" "trigonometry.5th" math

lit tau = 2 * math.pi
```
Once a file has been imported, subsequent imports are silently ignored.
To enable them, use `_` or a different identifier.

## Builtins

C programmers are likely used to writing preprocessor macros like 
`#define max( a, b ) ((a)>(b)?(a):(b))` or using function-like operators
like `sizeof`. Although it lacks a standard library, Fifth provides
many of these features through builtins -- pseudo-functions implemented
by the compiler.

`max` and `min` take any number of arguments of types `nat`, `int`, or `lit`, 
and return the maximum and minimum of the values, respectively.
```
max( 1, 3, 5 ) ⟷ 5
min( 1, 3, 5 ) ⟷ 1
```
`max` and `min` may also accept one argument of type `type`,
returning the maximum and minimum value expressable by that type.
```
max( nat8 ) ⟷ 255
min( int16 ) ⟷ -32768
```

`abs` takes one argument of types `nat`, `int`, or `lit`,
and returns its absolute value.
```
abs( -5 ) ⟷ 5
```

`sign` takes one argument of types `nat`, `int`, or `lit`,
and returns `-1` if it's less than zero, `1` if it's greater than zero,
or `0` otherwise.
```
sign( -5 ) ⟷ -1
sign( 0 ) ⟷ 0
sign( 5 ) ⟷ 1
```

The following static builtins are also provided.

`size` takes one argument of any type.
If it's a nonstatic type, it returns the width, in bytes, of the argument's type.
If it's the type `type`, it returns the width, in bytes, of the argument.
Otherwise, it returns 1.
```
size( int64( 5 ) ) ⟷ 8
size( nat32 ) ⟷ 4
size( 5 ) ⟷ 1
```

`type` takes one argument of any type, and returns its type.
```
type( 5 ) ⟷ lit
type( false ) ⟷ bool
type( nat8( 0 ) ) ⟷ nat8
```

`creak` and `croak` take zero or two arguments of type `static nat8 ptr` and `nat`.
When encountered by the compiler (e.g. in the taken branch of a static `if` statement),
they creak and croak, respectively, displaying their arguments as text.
```
croak()
creak( "Warning! 64-bit support is experimental!" )
croak( "Target must support RV32IMAC" )
```

`defined` takes one argument of "type" identifier.
It returns `true` if the identifier is defined in the current scope, or `false` otherwise.
```
defined( env ) ⟷ true
defined( env.debug ) ⟷ true
defined( true ) ⟷ true
defined( defined ) ⟷ true
```

TODO: add these builtins:

`pow`
`root`
`log`

`mean`
`hypot`

`clamp`
`lerp`

`gcd`
`lcm`

`roundup`
`rounddown`
`roundto`
`roundfrom`

`embed`

## Assertions #

Assertions are expressed with the `assert` keyword, followed by a boolean expression.
```
assert v == 5
```
When `env.debug` is `true`, assertion conditions are checked when reached at runtime.
If they fail, the program should emit some sort of error and call `abort`.

When `env.debug` is `false`, the behavior of a program failing an assertion is undefined.
The compiler is free to assume the assertion condition is always true, and optimize accordingly.

`assert` may be qualified as static with the `static` keyword. 
A static assertion is evaluated at compile time;
if it fails, or can't be evaluated, the assertion croaks and compilation halts.
It may be lowered as follows:
```
static assert x ⟷ static if not x: croak()
```
Perhaps one of Fifth's most powerful (and hardest to implement) features are type assertions,
which allow for the creation of new types with arbitrary undefined behaviors.
```
type nat4 = nat8 assert nat4 < 16
type even = nat assert even % 2 == 0
type prime = nat assert isprime( prime )
```
`expect` is `assert`'s younger sibling.
It lacks any semantic effect, but might be useful for branch prediction.
The `<hot>` and `<cold>` attributes may be used to specify exactly what
to expect.
```
expect q > 0
<hot> expect r[] in 'a'..'z'
<cold cold> expect t == 0x76C82BB1
```
`static expect` creaks upon failure.


## Inline Assembly #

Here's where Fifth gets interesting.

Fifth's inline assembly extension exposes
every RISC-V register as a variable, and
every instruction as a statement and a builtin.
This is useful for emitting instructions
the compiler is unable (or unwilling) to.
It's also a great way to shoot yourself in the foot.
```
$fence.i

$ebreak

nat64 cycles = $rdcycle()

r = $crc32.w( r )

$sp &= 0x12345678
```
While there are currently no concrete rules for exposing
arbitrary instructions in Fifth, there are some general guidelines.
Instructions using a destination register are typically exposed as builtins
returning that value.
Usually, R-type instructions will accept any
expression, while I-types will demand a static second operand
that can be expressed in twelve bits.
```
int rd, op1, op2 = _, 2, 3
$add rd, op1, op2
rd = $add( op1, op2 )

$add  rd, op1, z + 375 - f()
$addi rd, op1, 7 << 3
```

Blocks can be qualified with `$`.
Inside such a block, the `$` may be omitted from
assembly identifiers.
```
$ {
	TODO
}
```
Qualifying a block with *two* `$`
switches the syntax to that of assembly.
(Currently, this is a slight tweak of GNU `as`.)
```
$$ {
	TODO
}
```
Finally, qualifying a block with *three* `$`
additionally makes the assembly explicit; i.e.
exactly that sequence of instructions will appear
in the compiled binary.
```
$$$ {
	TODO
}
```
In any other context, assembly in Fifth is not explicit;
the compiler is free to modify your code to achieve a
semantically equivalent result.
With that said, there are some caveats --
for example, `$nop` is defined semantically
to always emit `addi x0, x0, 0`, 
so it can be used reliably in delay loops.

## Attributes

Attributes are qualifiers that appear in angle brackets.
Many are at least partially redundant, but they're useful shortcuts
for programmers (and implementors) who don't want to deal with
complex assertions or compiler flags to obtain good performance.
This is not an exhaustive list, although it might already be too long.

```
static		# <static> ⟷ static
constant	# akin to C's `const`, mostly for pointers
explicit	# akin to C's `volatile`

aligned
exclusive	# akin to C's `restrict`

tense		# suppress linker relaxation

hot		# optimize for speed, stackable
cold		# optimize for size, stackable

pure
leaf
unreturning	# akin to C's `_Noreturn`
unreachable

weak

interrupt
trap
naked		# no prologue/epilogue

inline		# ensure function/macro is inlined
outline		# ensure function/macro uses stack

deprecated	# creak when invoked
```

## Calling Convention

Given that Fifth's functions have features C lacks
(and vice versa),
a new calling convention might be in order.
Basically the only addition is that registers `a0`-`a7` are used for
return values as well as function arguments. On the other hand, the omissions
could be substantial, including variadic functions, structs/unions as arguments,
and soft floats. While these simplifications were a major goal of the language,
it's inevitable that people will demand to call `printf` from Fifth,
so they might end up as a future extension.

## Style

Fifth is pretty flexible in its formatting.
Here are two equally bad ways to write a factorial function:
```
# Computes the factorial of n.
#
# @param number: The number that goes in the factorial.
# @returns: The number that comes out of the factorial.
#
nat func computeFactorial( nat number ) {
	nat result = 1;
	for nat idx = 1; idx < number; idx = idx + 1 {
		result = result * idx;
	}
	return result;
}
```

```
#factorial function
nat r=1 func f nat n: for; n>1; n--; r*=n:;
```
Personally, I'd write it something like this.
```
# Compute `n` factorial without recursion.
# `n` must be < 12 to avoid overflow.
nat r func fact( nat n )
{
	assert n < 12 # 13! > max( nat )
	for r = 1; n > 1; n --: r *= n
}
```
But I honestly think it's pretty foolish
for anyone, most of all me, to
mandate a uniform code format
(as is in vogue these days).
At most, I hope that Fifth's grammar is
simple enough that organizations can write
their own styleguides and formatters.

# Example Code
Obviously, these are completely untested; most are adapted from C.
Hopefully, though, they offer some insight into what ideomatic Fifth (could) look like.

```
# rot13-encodes the null-terminated ASCII string pointed to by `s`.
func rot13( nat8 ptr s )
{
	for ; s[] != '\0'; s ++
	{
		if s[] in 'a'..'m', 'A'..'M': s[] += 13
		el s[] in 'n'..'z', 'N'..'Z': s[] -= 13
	}
}


# Returns the sum of the digits of `n` in radix `base` (by default, decimal).
( nat sum = 0 )func sumdigits( nat n, nat base = 10 )
{
	for ; n > 0; n /= base: sum += n % base
}


# Possible feature: function state global variables namespaced to their function?
nat8 ptr hexstr.s = make[ 9 ] '\0' # 9 bytes can store "FFFFFFFF\0"

# Returns a pointer to a null-terminated ASCII string containing `n` written in hexadecimal.
nat ptr func hexstr( nat n )
{
	for nat i in 7..0; n >>= 4
	{
		if n & 0xF < 10: s[ i ] = '0' + n & 0xF
		el:              s[ i ] = 'A' + n & 0xF - 10
	}
}


# Luhn tests the `n`-digit credit card number in the ASCII string pointed to by `c`.
bool passes luhn( nat8 ptr c, nat n )
{
	nat sum = 0
	for nat i = n - 1; i >= 0; i--
	{
		nat digit = c[ i ] - '0'
		if i % 2 != 0: sum += digit
		el digit < 5:  sum += 2 * digit
		el:            sum += 2 * digit - 9
	}
	return sum % 10 == 0
}


# Insertion-sorts the array of `n` nats pointed to by `a`.
func isort( nat ptr a, nat n )
{
	for nat i = 1; i < n; i ++
	{
		nat tmp, nat j = a[ i ], i
		for ; j > 0 and tmp < a[ j - 1 ]; j --
		{
			a[ j ] = a[ j - 1 ]
		}
		a[ j ] = tmp
	}
}


# Shellsorts the array of `n` nats pointed to by `a`.
func shellsort( nat ptr a, nat n )
{
	for nat gap in 701, 301, 132, 57, 23, 10, 4, 1
	{
		for nat i = gap; i < n; i++
		{
			nat tmp, nat j = a[ i ], i
			for ; j >= gap and a[ j - gap ] > tmp; j -= gap
			{
				a[ j ] = a[ j - gap ]
			}
			a[ j ] = tmp
		}
	}
}
```
