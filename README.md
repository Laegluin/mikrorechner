# Tmp

## Registers

- `r0` - `r31` (`000000-011111`): general purpose registers
- `null` (`100000`): register that always has the value `0`
- `addr_offset` (`100001`): register that stores an offset added to every absolute address

## Instruction Set

All instructions are 32-bit words. In all instructions, if the instruction takes more than
one register as argument, the register arguments are allowed to overlap. For example,
`add r0 r0 r0` is allowed and will double the value of `r0`.

### Arithmetic

```text
add: 0000 0DDD DDDA AAAA ABBB BBB* **** ****
     └────┘└─────┘└──────┘└─────┘
      op  dst-reg lhs-reg rhs-reg
```

```text
sub: 1011 1DDD DDDA AAAA ABBB BBB* **** ****
     └────┘└─────┘└──────┘└─────┘
      op  dst-reg lhs-reg rhs-reg
```

```text
mul: 0111 0DDD DDDA AAAA ABBB BBB* **** ****
     └────┘└─────┘└──────┘└─────┘
      op  dst-reg lhs-reg rhs-reg
```

```text
div: 0111 1DDD DDDA AAAA ABBB BBB* **** ****
     └────┘└─────┘└──────┘└─────┘
      op  dst-reg lhs-reg rhs-reg
```

Divides the value in `lhs-reg` by the value in `rhs-reg` and stores the result in `dst-reg`.
Division by zero causes the CPU to halt.

### Bitwise

```text
and: 1000 0DDD DDDA AAAA ABBB BBB* **** ****
     └────┘└─────┘└──────┘└─────┘
      op  dst-reg lhs-reg rhs-reg
```

```text
or: 1000 1DDD DDDA AAAA ABBB BBB* **** ****
    └────┘└─────┘└──────┘└─────┘
      op dst-reg lhs-reg rhs-reg
```

```text
not: 1001 0DDD DDDA AAAA A*** **** **** ****
     └────┘└─────┘└──────┘
      op   dst-reg  reg
```

```text
xor: 1001 1DDD DDDA AAAA ABBB BBB* **** ****
     └────┘└─────┘└──────┘└─────┘
      op  dst-reg lhs-reg rhs-reg
```

```text
shiftl: 1010 0DDD DDDA AAAA ABBB BBB* **** ****
        └────┘└─────┘└──────┘└─────┘
         op  dst-reg lhs-reg rhs-reg
```

```text
shiftr: 1010 1DDD DDDA AAAA ABBB BBB* **** ****
        └────┘└─────┘└──────┘└─────┘
         op  dst-reg lhs-reg rhs-reg
```

```text
signed_shiftr: 1011 0DDD DDDA AAAA ABBB BBB* **** ****
               └────┘└─────┘└──────┘└─────┘
                op  dst-reg lhs-reg rhs-reg
```

### Register

```text
copy: 0000 1DDD DDDS SSSS S*** **** **** ****
      └────┘└─────┘└──────┘
       op  dst-reg src-reg
```

```text
set: 0001 0DDD DDDI IIII IIII IIII IIII IIII
     └────┘└─────┘└────────────────────────┘
       op  dst-reg          value
```

### Comparison

Comparison instructions set or unset the comparison flag depending on the success/failure of the test.

```text
cmp_eq: 0001 1*** ***A AAAA ABBB BBB* **** ****
        └────┘       └──────┘└─────┘
          op         lhs-reg rhs-reg
```

```text
cmp_gt: 0010 0*** ***A AAAA ABBB BBB* **** ****
        └────┘       └──────┘└─────┘
          op         lhs-reg rhs-reg
```

```text
cmp_ge: 0010 1*** ***A AAAA ABBB BBB* **** ****
        └────┘       └──────┘└─────┘
          op         lhs-reg rhs-reg
```

### Jumps

There are two types of jumps:

- absolute: jump to the given absolute address
- relative: jump relative to the current instruction. The offset is added to the current
            instruction address (e.g. a relative jump with an offset of `0` jumps to the jump
            instruction itself, causing in infinite loop). Offsets are interpreted as a signed
            number in two's complement.

Both types come with a conditional variant. Conditional jumps jump only if the comparison flag is set;
otherwise they simply continue with the next instruction.

```text
jmp: 0011 0*** ***T TTTT T*** **** **** ****
     └────┘       └──────┘
       op       tgt-addr-reg
```

```text
jmp_rel: 0011 1III IIII IIII IIII IIII IIII IIII
         └────┘└───────────────────────────────┘
           op             offset
```

```text
jmp_if: 0100 0*** ***T TTTT T*** **** **** ****
        └────┘       └──────┘
          op       tgt-addr-reg
```

```text
jmp_rel_if: 0100 1III IIII IIII IIII IIII IIII IIII
            └────┘└───────────────────────────────┘
              op             offset
```

### Memory

Both instructions use byte-addressing. The offset is interpreted as unsigned integer that is
added to the base address.

```text
load: 0101 0DDD DDDS SSSS SIII IIII IIII IIII
      └────┘└─────┘└──────┘└────────────────┘
       op  dst-reg src-addr-reg  addr-offset
```

```text
store: 0101 1DDD DDDS SSSS SIII IIII IIII IIII
       └────┘└─────┘└──────┘└────────────────┘
       op dst-addr-reg src-reg  addr-offset
```

### Miscellaneous

```text
noop: 0110 0*** **** **** **** **** **** ****
      └────┘
        op
```

```text
halt: 0110 1*** **** **** **** **** **** ****
      └────┘
        op
```

## Assembler Syntax

```text
<R> ::= R1 | R2 | ... | R31 | null | offset
<I> ::= -<X> | ... | <X>
<X> ::= valid value in respect of the domain of command
```
### Kommentare
```text
Line comments are initiated with #.
```

### Labels
```text
<command> _<label> #<comment>      Initiation of labels
jump to <label>
jump_if to <label>
<R> = <label>        Store the adress of command or data in register
```

### Arithmetic

```text
add: <R> = <R> + <R>
sub: <R> = <R> - <R>
mul: <R> = <R> * <R>
div: <R> = <R> / <R>
```

### Bitwise

```text
and: <R> = <R> & <R>
or: <R> = <R> | <R>
xor: <R> = <R> ^ <R>
not: <R> = ~R
shiftleft: <R> = <R> << <R> times
shiftright: <R> = <R> >> <R> times
signed shift right: <R> = <R> >>_s <R> times
```

### Registers

```text
copy <R> to <R>
set: <R> = <I>
```

### Comparisons

```text
compare <R> = <R>
compare <R> > <R>
compare <R> >= <R>
```

### Jumps

```text
jump to <R>
jump_if to <R>
jump_rel to <I>
jump_rel_if to <I>
```

### Memory

```text
load <R> + <I> to <R>
store <R> + <I> to <R>
```
### Data

```text
Datastrings can be interpreted as Hex-, Binary or Decimal Numbers. 
The necessary number of Bytes for storage is adapted accordingly.
Hex-Strings: 0x<value>
Bin-Strings: 0b<value>
Decimal: <value>
```
### Miscellaneous

```text
halt
noop
```
