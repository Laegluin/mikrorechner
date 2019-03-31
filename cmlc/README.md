# cml

cml ist eine von F# und Rust inspirierte Hochsprache. Der Compiler `cmlc` erzeugt
ein ausführbares Speicherabbild oder eine Textdatei mit Assemblercode.

## Benutzung des Compilers

Ein beispielhafter Aufruf ist `cmlc main.cml -o main.img`. Die Ausgabe ist ein
Speicherabbild, das sowohl Instruktionen als auch Daten beinhaltet. Das Abbild
muss ab Adresse 0 ausgeführt werden, zum Beispiel mit dem Simulator: `simulator main.img`.
Wenn `--emit-asm` angegeben wird, ist die Ausgabe Assemblercode anstatt eines
Speicherabbildes. Eine genaue Auflistung aller Befehle wird mit `cmlc --help`
ausgegeben.

Der Compiler hat kein Konzept von mehreren Dateien oder einem Linker. Er kompiliert
genau eine Datei in genau ein direkt ausführbares Abbild. Direktes Kompilieren von
mehreren Dateien oder Linken von mehreren Abbildern ist nicht möglich.

## Grundlagen

Jedes cml-Programm beginnt mit einer `main`-Funktion:

```cml
fn main = {
    // hier kommt Code
}
```

`main` nimmt keine Argumente entgegen und gibt keine Werte zurück. Kommentare beginnen mit
`//` und enden am Ende der Zeile.

Variablen können mit `let` eingeführt werden:

```cml
let four = 2 + 2;
```

Variablen können nicht geändert werden, sobald sie zugwiesen wurden. Um eine Variable nach
der Definition zu mutieren, muss die Variable mit `mut` gebunden werden:

```cml
let a = 1;
a = 2;  // verboten

let mut a = 1;
a = 2;  // erlaubt, weil a als `mut` gebunden wurde
```

Die Mutierbarkeit ist transitiv. Wenn die Variable mutiert werden sollen, muss sie auf eine
mit `mut` deklarierte Bindung zurückgeführt werden können. Zum Beispiel darf der Inhalt eines
Pointers nur mutiert werden, wenn der Pointer selbst mutiert werden darf (mehr zu Pointern in
TODO: link)

Die Sichtbarkeit von Variablen ist lexikalisch, das heißt sie sind bis zum Ende eines Blocks
sichtbar. Definitionen können überschattet werden - in diesem Fall kann nicht mehr auf die
alte Variable zugegriffen werden.

```cml
let a = 2;

if true {
    let a = 3;
    // ab hier ist a = 3
}

// hier ist a = 2, weil der Block geschlossen wurde

let a = 4;
// hier ist a = 4, weil die Bindung überschattet wurde
// das alte a existiert aber immer noch (ein Pointer würde
// zum Beispiel immer noch auf 2 zeigen)
```

Abgesehen von `main` können weitere Funktionen definiert werden. Anders als zum Beispiel in C
ist die Deklarationsreihenfolge von Funktionen unwichtig. Der Compiler kann die Verweise auf
noch nicht deklarierte Funktionen richtig auflösen. Funktionen können beliebig viele (oder gar
keine, wie `main`) Parameter deklarieren.

Eine Funktion hat immer genau einen Rückgabewert. Die obige Einführung von `main` war also nicht
ganz korrekt: `main` gibt `()` (ausgesprochen 'unit') zurück. Unit ist ein leeres Tupel und hat
somit immer den Wert `()`.

```cml
fn add l, r = {
    l + r
}
```

Es nicht notwendig ist, den letzten Wert eines Blocks zurückzugeben. Das liegt daran, dass cml
Expression-basiert ist. Jedes Konstrukt ist eine Expression, die zu einem Wert evaluiert wird.
Ein Block evaluiert zur letzten Expression, falls die Expression nicht durch ein `;` terminiert
wird. Andernfalls evaluiert ein Block zu `()`. Eine Funktion wiederum gibt den Wert zurück, zu
dem der Block der Funktion evaluiert.

Werte können auch mit dem `ret`-Schlüsselwort zurückgegeben werden. Die Expression, die `ret`
folgt, wird zurückgegeben. Falls keine Expression angegeben ist, wird `()` zurückgeben. `ret;`
und `ret ();` sind also äquivalent. Aufgrund der Expression-basierten Natur von cml sind explizite
`ret`s nur selten notwendig und sollten vermieden werden. Sie sind aber nützlich, um frühzeitig
aus einer Funktion zurückzukehren.

```cml
fn div_or_zero l, r = {
    if r == 0 {
        ret 0;
    }

    l / r
}
```

Funktionen mit mindestens einem Parameter werden mit dem Namen gefolgt von einem `:` aufgerufen.
Argumente werden mit einem Komma getrennt und können optional mit Parameternamen versehen werden.
Funktionen ohne Parameter werden mit `!` aufgerufen.

```cml
fn no_params = {}

fn add l, r = {
    l + r
}

fn main = {
    // Aufruf ohne Argumente
    no_params!;

    // Aufruf mit zwei Argumenten
    let three = add: 1, 2;

    // Aufruf mit benannten Parametern
    // die Reihenfolge wird durch die Namen bestimmt
    let four = add: r = 2, l = 2;
}
```

## Typsystem

Obwohl in den obigen Beispielen keine Typen angegeben wurden, ist cml eine statisch typisierte
Sprache. Der Compiler benutzt einen auf dem Algorithmus W für [Hindley-Milner Typsysteme][hindley-milner]
basierenden Typinferenzalgorithmus, um die Typen für Funktionen und Variablen zu bestimmen.

Typen können aber auch explizit angegeben werden, um dem Compiler zu helfen und
Funktionen zu dokumentieren. Für Variablen und Parameter folgen Typangaben einem
`:`. Rückgabetypen für Funktionen werden mit `->` notiert. Eine vollständig annotierte
Funktion sieht zum Beispiel wie folgt aus:

```cml
fn div_double l: i32, r: i32 -> i32 = {
    let double: i32 = 2 * l;
    double / r
}
```

Die Grundlage des Typsystems machen die primitiven Typen aus:

- `!`, 'never'
- `()`, 'unit'
- `bool`, ein boolscher Wert, entweder `true` oder `false`
- `i32`, ein 32-bit Integer mit Vorzeichen (Zweierkomplement)
- `u32`, ein 32-bit Integer ohne Vorzeichen
- `str`, ein beliebig großer UTF-8 String
- `*T`, ein Pointer auf einen Typen `T`
- `*mut T`, ein Pointer auf einen Typen `T`, der Mutation erlaubt
- `[T; N]`, ein Array mit `N` Elementen vom Typ `T`
- `(T, U, ...)`, ein Tupel mit Elementen von Type `T`, `U`, usw.
- `fn T -> R`, eine Funktion mit einem Parameter vom Typ `T` und dem Rückgabetyp `R`
  (die Anzahl der Parametertypen ist variabel)

Außerdem können benutzerdefinierte Records (ähnlich zu `struct`s in C) als Typen definiert werden.

Typkonstruktoren für generische benutzerdefinierte Typen werden nicht unterstützt. Generische Funktionen
sind ebenfalls nicht erlaubt. Unterstützung für beide ist aber prinzipiell mit dem jetzigem Typsystem
und Typinferenzalgorithmus möglich. Für die Code-Generierung würde es sich anbieten
[Monomorphisierung][monomorphization] zu verwenden.

### Never `!`

`!` oder 'never' ist ein besonderer Typ, der nicht explizit als Annotation verwendet werden kann.
`!` ist der Typ einer divergierenden Berechnung, also einer Berechnung die niemals durchgeführt
wird. `!` macht Sinn, wenn man bedenkt, das cml Expression-basiert ist: `!` ist der Typ einer
`ret`-Expression.

`!` hat einige interessante Eigenschaften. Aus der Definition folgt, dass es keine Werte vom Typ `!`
gibt. Trotzdem ist jeder Typ mit dem Typen `!` kompatibel, weil eine Zuweisung niemals erfolgen kann.
Im Sinne des Typsystems ist `!` notwendig, um `ret`-Expressions ohne Sonderfälle zu erlauben. Ohne
`!` würde ein `ret` zu einem Typfehler führen, weil der Typ der momentanen Expression nicht der
Rückgabetyp wäre.

`!` wäre auch für andere Kontrollflussprobleme nützlich, zum Beispiel für ein `break` in einem Loop
oder aber zur korrekten Typisierung eines Exception-Mechanismus.

`!` wird allgemein auch als [Bottom-Typ][bottom-type] bezeichnet.

### Unit `()`

`()` oder 'unit' ist ein Typ der nur genau einen Wert annehmen kann: `()`. Ein Wert vom `()` hält
also keine Information. Deswegen kann der Compiler während der Codegenerierung alle Variablen vom
Typ `()` vollkommen ignorieren. Aus diesem Grund ist `()` der Rückgabetyp von Funktionen, die keine
Werte zurückgeben und der Typ von leeren Blöcken und Blöcken, in denen der Wert der letzten Expression
mit einem `;` explizit verworfen wird.

`()` kann auch als 0-Tupel betrachtet werden. Tatsächlich entspricht dies auch der Implementation
im Compiler.

`()` erfüllt den selben Zweck wie `void` in C. Da `()` aber ein vollwertiger Typ ist, sind weniger
Sonderfälle im Compiler notwendig. Insbesondere gibt es keine Funktionen ohne Rückgabewert. Wenn
cml generische Typen hätte, würde `()` außerdem besser mit generischen Signaturen zusammenarbeiten,
weil es als Typ eine gültige Belegung für einen Typparameter ist.

Siehe auch <https://en.wikipedia.org/wiki/Unit_type>.

### Bool

`bool` ist der Typ für boolsche Variablen. Ein Variable vom Typ `bool` kann die Werte `true` oder
`false` annehmen. `bool`s werden für die Bedingungen in `if`- und `while`-Expressions verwendet.

Mit Werten von diesem Typ können die Operatoren `&&` (logisches und), `||` (logisches oder) und
`!` (logische Negation) benutzt werden. `&&` und `||` evaluieren beide Operanden. Es wäre sinnvoll,
nach der Evaluation vom ersten Operanden zu entscheiden, ob der zweite Operand für das Ergebnis
überhaupt noch relevant ist.

### Numerische Typen

`u32` und `i32` sind 32-bit Integer. `u32` besitzt kein Vorzeichen, `i32` kodiert ein Vorzeichen im
Zweierkomplement.

Mit allen numerischen Typen können folgende Operatoren verwendet werden:

- `==`: Gleichheit
- `!=`: Ungleichheit
- `>`: größer als
- `>=`: größer gleich
- `<`: kleiner als
- `<=`: kleiner gleich
- `-`: Subtraktion/Negation (Subtraktion hat höhere Präzedenz)
- `+`: Addition
- `*`: Multiplikation
- `/`: Division

Die Operatoren entsprechen direkt den jeweiligen Instruktionen auf der Hardware. Entsprechend
werden die jeweiligen Eigentschaften übernommen: bei Overflow wird wraparound benutzt, Division
durch 0 führt zu einem Trap.

### Strings

### Pointer

### Arrays

### Tupel

### Funktionen

Type-Casts
*mut aliasing
Nominal-Typing
Type-Alias
Patterns
Function named-args resolution order
Method-Calls
First-Class Functions (arg names)
Inferenz
Operatoren (div by zero, Overflow)
Expr-basiert
Safety (Ptr)
Stacksetup
Calling-Convention
Beispiele
Operator-Präzedenz

[hindley-milner]: https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system
[monomorphization]: https://doc.rust-lang.org/1.30.0/book/2018-edition/ch10-01-syntax.html?highlight=mono#performance-of-code-using-generics
[bottom-type]: https://en.wikipedia.org/wiki/Bottom_type
