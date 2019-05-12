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

Einige Beispiele finden sich im [Git-Repository][repo] unter `cmlc/tests`.

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
Pointers nur mutiert werden, wenn der Pointer selbst mutiert werden darf (siehe [Pointer](#pointer))

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

Eine besondere Typ-Annotation ist `_`. `_` weist den Compiler an, den Typen zu inferieren. Dies ist nützlich,
um nur einen Teil eines Typkonstruktors explizit einzuschränken. `*_` zum Beispiel annotiert den Typ als
Pointer auf einen beliebigen Typ.

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
Ret-Expression.

`!` hat einige interessante Eigenschaften. Aus der Definition folgt, dass es keine Werte vom Typ `!`
gibt. Trotzdem ist jeder Typ mit dem Typen `!` kompatibel, weil eine Zuweisung niemals erfolgen kann.
Im Sinne des Typsystems ist `!` notwendig, um Ret-Expressions ohne Sonderfälle zu erlauben. Ohne
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
`false` annehmen. `bool`s werden für die Bedingungen in `if`- und While-Expressions verwendet.

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
- `-`: Subtraktion/Negation (Negation hat höhere Präzedenz)
- `+`: Addition
- `*`: Multiplikation
- `/`: Division

Die Operatoren entsprechen direkt den jeweiligen Instruktionen auf der Hardware. Entsprechend
werden die jeweiligen Eigentschaften übernommen: bei Overflow wird wraparound benutzt, Division
durch 0 führt zu einem Trap.

### Strings

Strings sind Bytesequenzen, die immer als gültiges UTF-8 kodiert sind. Der Typ eines Strings ist
`str`. Weil Strings beliebig lang sind, müssen sie über einen Pointer referenziert werden; in
der Regel werden Strings mit einem `*str` verwendet.

Anders als zum Beispiel in C sind Strings nicht null-terminiert. Momentan muss die Länge deswegen
separat gespeichert werden. Das Ziel ist, die Länge mit dem Pointer auf den String zu speichern.
Ein `*str` ist dann ein 'fat pointer', bestehend aus zwei Maschinenworten: Adresse + Länge. Eine
solche Implementation lässt sich auch für dynamische Arrays verwenden. Der Vorteil dieser Repräsentation
ist, dass sie auch für Teilsequenzen einer größeren Sequenz verwendet werden kann.

Strings sind in der momentanen Implementation größtenteils nutzlos, weil es keine Operationen gibt,
die auf Strings angewendet werden kann. Dies liegt vor allem daran, dass der Instruktionssatz
keine Instruktionen für den Umgang mit einzelnen Bytes besitzt. Außerdem müsste `char` als
primitiver Typ hinzugefügt werden. Ein `char` sollte einen Unicode-Codepoint (also 32 bit) darstellen.

### Pointer

Pointer kommen in zwei Varianten: `*T` und `*mut T`. Beide zeigen auf einen Wert vom Typ `T`. `*T`
Pointer erlauben nur lesenden Zugriff, während `*mut T` Pointer auch schreibenden Zugriff erlauben.

Pointer können mit den beiden address-of-Operatoren erzeugt werden: `&value` für `*T` und `&mut value`
für `*mut T`. Es gibt keine Beschränkung für die Erzeugung von Pointern. Das heißt insbesondere, dass
es mehr als einen `*mut T` Pointer auf den gleichen Wert geben kann oder ein `*T` und ein `*mut T`
Pointer. Ein `*T` Pointer gibt also keine Garantie, dass der Wert nicht verändert werden kann, sondern
stellt nur sicher, das eine Änderung nicht durch diesen Pointer geschehen kann.

### Arrays

Arrays haben den Typ `[T; N]`, wobei `T` der Typ der Elemente und `N` eine konstante positive Zahl ist,
die die Länge des Arrays festlegt. Arrays verschiedener Längen sind also nicht mit einander kompatibel.

Arrays werden fortlaufend auf dem Stack gespeichert, ein Element nach dem anderen ohne zusätzliches
Padding.

Arrays werden mit Array-Expressions erzeugt. Es gibt zur Zeit keinen Index Operator, auf einzelne Elemente
muss deswegen mit Pointer-Arithmetik und Typ-Casts zugegriffen werden.

```cml
// ein Array mit sechs Elementen
let fib: [u32; 6] = [1, 1, 2, 3, 5, 8];

// Zugriff das dritte (Index 2) Element:
// - Adresse von Array als u32 casten
// - Adresse für drittes Element berechnen
// - u32 als Pointer casten und den Pointer dereferenzieren
let third_element = *(((&fib as u32) + 2 * 4) as *u32);
```

### Tupel

Tupel sind eine heterogene Sammlung von verschiedenen Typen.`(u32, bool)` zum Beispiel, ist ein Tupel
bestehend aus einem Integer und einem Boolean. Einzelne Felder eines Tupels haben keine Namen und
können nicht direkt adressiert werden. Sie müssen in einem `let`-Bindung destrukturiert werden.

```cml
// erzeugt ein Tupel vom Typ (bool, i32, *str)
let tuple = (false, 1, "two");

// destrukturiert das Tupel und bindet die einzelnen Felder
let (boolean, number, string) = tuple;
```

### Funktionen

Der Typ von Funktionen wird über die Typen der Parameter und den Rückgabetypen definiert. Eine Funktion
die keine Argumente entgegennimmt und einen Integer zurückgibt, hat den Typen `fn -> i32`, eine Funktion
die zwei Integer addiert, hat den Typen `fn i32, i32 -> i32`.

Genau wie Strings können Funktionen nicht direkt als Werte gebunden werden, sondern müssen über einen
Pointer referenziert werden. Wenn eine Funktion definiert wird, wird auch ein Pointer auf diese unter dem
Namen der Funktion gebunden.

```cml
// die Definition der Funktion bindet einen Pointer auf diese als `add_twice`
fn add_twice left: i32, right: i32 -> i32 = {
    left + right + right
}

fn main = {
    // der Pointer auf `add_twice` entspricht dem Typen der Signatur
    let func: *fn i32, i32 -> i32 = add_twice;
}
```

### Records

Records sind benutzerdefinierbare Typen (ähnlich zu structs in C). Sie erlauben es Daten in benannten
Feldern zu gruppieren. Felder werden linear im Speicher ausgelegt, abhängig von den Typen der Felder
(die wiederum Records sein können). Aus diesem Grund müssen alle Felder eine feste Größe haben und
dürfen nicht rekursiv auf den definierten Typen verweisen. In diesem Fall muss das Feld mit einem Pointer
auf den Wert verweisen. Der Pointer hat immer eine feste Größe, unabhängig vom Typ des Wertes auf den er zeigt.

```cml
// definiert einen Record mit den Feldern `x` und `y`
type Vector = {
    x: u32,
    y: u32,
}
```

Jeder Record definiert einen neuen nominalen Typen. Das bedeutet, dass die Typen zweier Records nicht
gleich sind, selbst wenn alle Felder gleich sind. Der Name allein definiert die Identität des Typs.

```cml
type Point = {
    x: u32,
    y: u32,
}

fn main = {
    // die Typen sind nicht kompatibel, obwohl die Felder gleich sind,
    // weil die Namen der Typen unterschiedlich sind
    let vec: Vector = Point: x = 1, y = 2;
}
```

Wenn ein Record definiert wird, erzeugt der Compiler eine Konstruktor-Funktion mit dem Namen des Records.
Die Signatur des Konstruktors entspricht den Feldern: für jedes Feld besitzt der Konstruktor einen Parameter
mit dem Namen und Typen des Felds.

```cml
type Slice = {
    start: *u32,
    len: u32,
}

// der Compiler erzeugt eine vergleichbare Konstruktor-Funktion für `Slice`:
fn Slice start: *u32, len: u32 -> Slice = {
    // Compiler-Builtin
}
```

Momentan erzeugt der Compiler einen Konstruktor der alle Argumente in den Rückgabewert kopiert. Als zukünftige
Verbesserung ist es sinnvoll, alle direkten Aufrufe des Konstruktors zu inlinen, um den Overhead eines
Funktionsaufrufs wenn möglich zu vermeiden.

### Typaliasse

Für Typen, deren Namen lang sind, können Aliasse erstellt werden. Ein Alias verhält sich identisch zum Typen auf
den er verweist und kann überall an seiner Stelle verwendet werden.

```cml
// erlaubt den Funktionspointer unter dem Namen `FindPred` zu verwenden
type FindPred = *fn u32 -> bool;
```

### Typ-Casts

Einfache numerische Typen, also `u32`, `i32`, `*T` und `*mut T` können mit dem `as` Operator ineinander umgewandelt
werden. Casts sind nur eine Anweisung an den Compiler, den Typen eines Wertes anders zu interpretieren. Zur Laufzeit
gibt es keinen Unterschied. Insbesondere heißt dies, dass bei Casts zwischen vorzeichenbehafteten bzw. vorzeichenlosen
Datentypen der Wert als Zweierkomplement uminterpretiert wird. Beim Casten auf einen Pointer wird das Bitmuster
als Adresse interpretiert.

```cml
let a: i32 = -1;
let b = a as u32; // b hat den Wert 4294967295
```

## Syntax und Semantik

### Kontrollfluss

#### If-Expressions

If-Expressions erlauben es Entscheidungen zu treffen. Eine If-Expression besteht aus drei Teilen: Der Bedingung,
dem Then-Block und dem Else-Block. Der Else-Block ist optional und kann auch durch eine weitere If-Expression
ausgetauscht werden.

```cml
if true {
    // erster Fall
} else if false {
    // zweiter Fall
}

// äquivalent zu:
if true {
    // erster Fall
} else {
    if false {
        // zweiter Fall
    }
}
```

Wenn eine If-Expression keinen Else-Block besitzt ist der Typ und Wert `()`. Wenn ein Else-Block vorhanden ist,
müssen sowohl Then- als auch Else-Block den selben Typen besitzen. In diesem Fall ist der Typ der If-Expression
der Typ der beiden Blöcke und der Wert der Wert des Blocks, der ausgeführt wurde.

```cml
// weil `if` eine Expression ist, kann es wie der ternäre Operator in C verwendet werden
let sign = if num < 0 { -1 } else if num > 0 { 1 } else { 0 };
```

#### While-Loops

While-Loops führen Loop-Block solange aus, bis die Bedingung zu `false` evaluiert. Die Bedingung wird vor der
Ausführung des Blocks geprüft. Einen Do-While-Loop gibt es nicht. Typ und Wert eines While-Loops ist immer `()`.

```cml
// ein einfacher While-Loop der zehn Iterationen durchläuft
let mut i = 0;
while i < 10 {
    i = i + 1;
}
```

Momentan sind keine `break` oder `continue` Anweisungen implementiert. Derartige Funktionalität muss deswegen
manuell emuliert werden.

#### Ret-Expressions

Ret-Expressions erlauben es sofort einen Wert aus einer Funktion zurückzugeben. Wenn der Rückgabewert `()` ist,
muss nach dem `ret` Schlüsselwort kein Wert angegeben werden. Der Typ einer Ret-Expression ist `!` und somit mit
jeder Zuweisung kompatibel (siehe [`!`-Typ](#never)).

```cml
fn main = {
    // Kurzform für `ret ()`
    ret
}
```

### Zuweisungen

Zuweisungen sind immer eine bitweise Kopie des zugewiesenen Werts in die neue Variable. Da die meisten Werte
in der Regel direkt auf dem Stack gespeichert werden, entspricht dies meistens einer vollständigen Kopie. Eine
signifikante Ausnahme sind Pointer oder Werte die Pointer beinhalten: da die Kopie bitweise ist, wird nur der
Pointer, nicht aber das Ziel, auf das der Pointer zeigt, kopiert.

```cml
let a = 42;
let b = a; // Kopie von `a`, also auch `42`

let b_ptr = &b;
let copy = b_ptr; // Kopie des Pointers, aber beide zeigen auf `b`
```

### Block-Expressions

Blöcke ermöglichen die sequentielle Ausführung von mehreren, durch `;` separierten Expressions nacheinander.
Der Typ und Wert der letzten Expression werden für den Block übernommen. Wenn die letzte Expression durch ein
`;` terminiert wird oder der Block leer ist, evaluiert der Block stattdessen zu `()`.

Eine Ausnahme bilden Ret-Expressions. Wenn die letzte Expression ein `ret` ist, wird immer der Typ übernommen,
auch wenn ein `;` folgt (also `!`). Auf diese Weise werden Typfehler verhindert, die keinen Sinn machen würden,
da das Ende des Blocks offensichtlich nie erreicht wird.

```cml
// die meisten Blöcke sind als Körper von Funktionen antreffbar
// in diesem Fall hat der Block den Typ `()`
fn main = {
    // Blöcke können auch verwendet werden, um die Sichtbarkeit von Variablen
    // einzuschränken
    let a = {
        let int = returns_int!;
        int + 3
    };
}

// kein `;` am Ende, also ist der Typ `i32`
fn returns_int -> i32 = {
    let one = 1;
    one + 1
}
```

### Let-Expressions

Let-Expressions dienen primär dazu, Werte an Variablen zu binden. Neue Variablen können alte überschatten.
In diesem Fall kann nicht mehr direkt auf die alte Variable zugegriffen werden.

Genau genommen bestehen Let-Expressions aus einem Pattern und einem Wert, der an das Pattern gebunden wird.
Es gibt vier verschiedene Patterns:

- `_` (Ignorieren):
  
  Ignoriert den Wert vollständig. An sich ist dieses Pattern nicht sonderlich nützlich, es kann aber verwendet
  werden, um Teile eines komplizierteren Patterns zu ignorieren. Zum Beispiel:

  ```cml
  let _ = "unwichtig";
  ```

- Variable:

  Ein Variablen-Pattern bindet den Wert an eine Variable mit dem gegebenen Namen. Dies ist das häufigste Pattern,
  das für normale Zuweisungen verwendet werden kann. Zum Beispiel

  ```cml
  let x = 2;
  ```

- `mut`-Variable:

  Wenn vor dem Variablennamen ein `mut` steht, ist die gebundene Variable mutierbar. Zum Beispiel:

  ```cml
  let mut a = 1;
  a = 2;
  ```

- Tupel

  Bindet die einzelnen Felder eines Tupels. Jedes Feld kann wiederum aus einem beliebigen Pattern bestehen.
  Zum Beispiel:

  ```cml
  let (a, b, c) = (1, 2, 3);
  ```

Da Patterns miteinander kombiniert werden können, können beliebig komplexe Patterns verwendet werden, um Daten zu
destrukturieren:

```cml
let complex_tuple = ("hey", (-4, true, false, (1, 1)));
let (a, (mut b, _, c, (x, y))) = complex_tuple;
```

### Benannte Funktionsparameter

Bindung, Auflösungsreihenfolge

### Methodenaufrufe

### Operator-Präzedenz

Die folgende Liste ordnet alle Operatoren nach Präzedenz, beginnend mit der niedrigsten Präzedenz:

- `=` (Zuweisung), binär, rechts-assoziativ
- `||`, binär, links-assoziativ
- `&&`, binär, links-assoziativ
- `==`, binär, links-assoziativ
- `>`, `>=`, `<`, `<=`, binär, links-assoziativ
- `+`, `-`, binär, links-assoziativ
- `*`, `/`, binär, links-assoziativ
- `as`, unär, links-assoziativ
- `-` (Negation), `!`, `*`, `&`, `&mut`, unär, rechts-assoziativ

## Code-Generierung und ABI

### Laufzeitinitialisierung

### Callstack

### Calling convention

[repo]: https://github.com/Laegluin/mikrorechner
[hindley-milner]: https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system
[monomorphization]: https://doc.rust-lang.org/1.30.0/book/2018-edition/ch10-01-syntax.html#performance-of-code-using-generics
[bottom-type]: https://en.wikipedia.org/wiki/Bottom_type
