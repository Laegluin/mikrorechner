# simulator

Der Simulator erlaubt es Maschinencode auszuführen und interaktiv zu debuggen.

Er ist vor allem dafür gedacht Programme zu testen. Es ist kein Ziel, das Verhalten der Hardware
vollständig zu imitieren. Im Gegenteil, während korrekte Programme natürlich korrekt simuliert werden
sollen, sollen Fehler frühzeitig erkannt werden.

Dies beeinflusst vor allem das Speichermodell. Der Simulator benutzt eine 2-stufige Pagetabelle für den
simulierten Speicher. Die ersten 10 bit einer Adresse sind der Index in die erste Pagetabelle, die nächsten
10 bit sind der Index auf die nächste Pagetabelle und die letzten 12 bit sind die Adresse innerhalb der
Page (jede Page ist 4 KiB groß). Paging macht es möglich, nicht den gesamten Adressraum zu initialisieren,
was auf modernen 64 bit Systemen zwar möglich, aber verschwenderisch ist.

Wenn eine Page beschrieben wird, wird sie mit einem Zufallswert beschrieben, um nicht initialisierten Speicher
zu simulieren. Falls eine Page geladen werden soll, die noch nicht initialisiert wurde, kann der Simulator
aber direkt mit einem Fehler abbrechen.

Hazards sind momentan nicht implementiert. Ziel wäre aber eine einfache Implementation, die anhand der
auszuführenden und bereits ausgeführten Instruktionen Hazards erkennt und mit einem entsprechenden Fehler
abbricht. Die Effekte eines Hazards tatsächlich zu simulieren hat wenig Sinn, da ein Auftreten sowieso ein
Fehler im Programm ist.

## Start des Simulators

Zum Starten des Simulators muss der Pfad zu einem Speicherabbild angegeben werden, zum Beispiel: `simulator mem.img`.
Das Abbild wird direkt in den Speicher eingelesen und ausgeführt, beginnend bei Adresse 0. Zum Debuggen kann es
nützlich sein, das Flag `--start-paused` zu übergeben, um eine direkte Ausführung zu verhindern.

Alternativ zu einem binären Speicherabbild kann auch eine Textdatei angegeben werden. Dazu muss das Flag
`--convert-from-text` angegeben werden. In der Datei kann eine Instruktion pro Zeile angegeben werden, als
Folge von Einsen und Nullen. Die Folge wird als eine 32 bit Zahl interpretiert, Endianness ist also nicht relevant.
Leerzeichen, leere Zeilen oder Zeilen beginnend mit `#` werden ignoriert.

Alle Optionen mit Beschreibung können mit `simulator --help` ausgegeben werden.

## Interaktive Benutzung

Nach dem Start wird die interaktive Konsole des Simulators geöffnet. Während die Simulation aktiv ist, werden
Statusnachrichten nach einem `▶` angezeigt. `p` oder `pause` pausiert die Simulation, `c` oder `continue` setzt
sie fort.

Während die Simulation pausiert ist, kann der Inhalt der verschiedenen Register, Flags und des Speichers ausgegeben
werden. `reg r31` gibt zum Beispiel den Inhalt von Register R31 aus. Mit `set_break` können Haltepunkte gesetzt
werden, an denen die Simulation automatisch pausiert wird. `enable_trace` erlaubt die Ausgabe aller ausgeführten
Instruktionen in einem menschenlesbaren Format. Für eine genaue Beschreibung aller Befehle kann `?` oder `help`
verwendet werden.
