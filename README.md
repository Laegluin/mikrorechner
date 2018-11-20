# Tmp

## Registers

- `r0` - `r31` (`000000-011111`): general purpose registers
- `null` (`100000`): register that always has the value `0`
- `addr_offset` (`100001`): register that stores an offset added to every absolute address

## Instruction Set

All instructions are 32-bit words.

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
            instruction itself, causing in infinite loop)

Both types come with a conditional variant. Conditional jumps jump only if the comparison flag is set;
otherwise they simply continue with the next instruction.

```text
jmp: 0011 0TTT TTT* **** **** **** **** ****
     └────┘└─────┘
       op  tgt-addr-reg
```

```text
jmp_rel: 0011 1III IIII IIII IIII IIII IIII IIII
         └────┘└───────────────────────────────┘
           op             offset
```

```text
jmp_if: 0100 0TTT TTT* **** **** **** **** ****
        └────┘└─────┘
          op  tgt-addr-reg
```

```text
jmp_rel_if: 0100 1III IIII IIII IIII IIII IIII IIII
            └────┘└───────────────────────────────┘
              op             offset
```

### Memory

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
