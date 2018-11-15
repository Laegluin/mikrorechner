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
    OP-code dst-reg lhs-reg rhs-reg
```

```text
mul: 0111 0DDD DDDA AAAA ABBB BBB* **** ****
     └────┘└─────┘└──────┘└─────┘
    OP-code dst-reg lhs-reg rhs-reg
```

```text
div: 0111 1DDD DDDA AAAA ABBB BBB* **** ****
     └────┘└─────┘└──────┘└─────┘
    OP-code dst-reg lhs-reg rhs-reg
```

### Bitwise

```text
and: 1000 0DDD DDDA AAAA ABBB BBB* **** ****
     └────┘└─────┘└──────┘└─────┘
    OP-code dst-reg lhs-reg rhs-reg
```

```text
or: 1000 1DDD DDDA AAAA ABBB BBB* **** ****
    └────┘└─────┘└──────┘└─────┘
   OP-code dst-reg lhs-reg rhs-reg
```

```text
not: 1001 0DDD DDDA AAAA A*** **** **** ****
     └────┘└─────┘└──────┘
    OP-code dst-reg  reg
```

```text
xor: 1001 1DDD DDDA AAAA ABBB BBB* **** ****
     └────┘└─────┘└──────┘└─────┘
    OP-code dst-reg lhs-reg rhs-reg
```

```text
shiftl: 1010 0DDD DDDA AAAA ABBB BBB* **** ****
        └────┘└─────┘└──────┘└─────┘
       OP-code dst-reg lhs-reg rhs-reg
```

```text
shiftr: 1010 1DDD DDDA AAAA ABBB BBB* **** ****
        └────┘└─────┘└──────┘└─────┘
       OP-code dst-reg lhs-reg rhs-reg
```

### Register

```text
copy: 0000 1DDD DDDS SSSS S*** **** **** ****
      └────┘└─────┘└──────┘
     OP-code dst-reg src-reg
```

```text
set: 0001 0DDD DDDI IIII IIII IIII IIII IIII
     └────┘└─────┘└────────────────────────┘
    OP-code dst-reg         value
```

### Comparison

Comparison instructions set or unset the comparison flag depending on the success/failure of the test.

```text
cmp_eq: 0001 1*** ***A AAAA ABBB BBB* **** ****
        └────┘       └──────┘└─────┘
        OP-code      lhs-reg rhs-reg
```

```text
cmp_gt: 0010 0*** ***A AAAA ABBB BBB* **** ****
        └────┘       └──────┘└─────┘
        OP-code      lhs-reg rhs-reg
```

```text
cmp_ge: 0010 1*** ***A AAAA ABBB BBB* **** ****
        └────┘       └──────┘└─────┘
        OP-code      lhs-reg rhs-reg
```

### Jumps

```text
jmp: 0011 0TTT TTT* **** **** **** **** ****
     └────┘└─────┘
   OP-code tgt-addr-reg
```

```text
jmp_rel: 0011 1III IIII IIII IIII IIII IIII IIII
         └────┘└───────────────────────────────┘
         OP-code            offset
```

Conditional jumps jump to the target address if the comparison flag is set or continue execution otherwise.

```text
jmp_if: 0100 0TTT TTT* **** **** **** **** ****
        └────┘└─────┘
      OP-code tgt-addr-reg
```

```text
jmp_rel_if: 0100 1III IIII IIII IIII IIII IIII IIII
            └────┘└───────────────────────────────┘
            OP-code            offset
```

### Memory

```text
load: 0101 0DDD DDDS SSSS SIII IIII IIII IIII
      └────┘└─────┘└──────┘└────────────────┘
    OP-code dst-reg src-addr-reg  addr-offset
```

```text
store: 0101 1DDD DDDS SSSS SIII IIII IIII IIII
       └────┘└─────┘└──────┘└────────────────┘
    OP-code dst-addr-reg src-reg  addr-offset
```

### Miscellaneous

```text
noop: 0110 0*** **** **** **** **** **** ****
      └────┘
      OP-code
```

```text
halt: 0110 1*** **** **** **** **** **** ****
      └────┘
      OP-code
```
