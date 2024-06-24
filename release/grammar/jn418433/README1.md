## Dane podstawowe
Nazwa języka: *Shiro*
Rozszerzenie plików: `.shr`
Typ języka: imperatywny
## Specyfikacja

### Typy, zmienne, stałe, literały, przekazywanie argumentów

Język posiada trzy typy: `Int`, `Bool`, `String`, `Unit`. 
`Unit` to pseudo typ zwracany precyzujący "funkcję" nie zwracającą wartości.
Typy `Int`, `Bool`, `String` to typy przyjmujące wartości.
Zmienne zawsze posiadają wartość. I deklaruje się je w następujący sposób
```kotlin
val nazwaZmiennej : String = "nowy napis";
val automatycznyInt = 3;
val automatycznyBool = True;
```

Typy posiadają literały. 
`Int` - liczby całkowite, 
`Boolean` - `True` i `False`, `
`String` - ciągi znaków w cudzysłowach.

Na liczbach można wykonywać operacje arytmetyczne: `+`, `-`, `*`, `/`, `%`. 
Na wartościach logicznych można wykonywać operacje logiczne: `&&`, `||`, `!`. 
Na łańcuchach znaków można wykonywać operacje konkatenacji: `+`. Na `Integer` można wykonywać operacje porównania: ` == `, `!=`, `<`, `>`, `<=`, `>=`.


Standardowe przekazania zmiennej do funkcji odbywa się przez wartość, prefiksując typ argumentu słowem kluczowym `const` lub/i `ref` możemy wymusić przekazanie argumentu przez referencję lib/i wymusić zachowanie niemutowalność argumentu w ciele funkcji.

W języku

```kotlin
fun foo(mutableArgAuto, constInt: ref Int, constRefString : const ref String) {
	...
}
```

Interpreter nie wymaga podawania typów przy deklarowaniu zmiennych i argumentów funkcji. Wymaga jedynie podania typu wartości zwracanej przez funkcję.

## Funkcje standardowe


```kotlin
printInt 3 + 3;
// 6
printBool True;
// True
printString "String";
// String
```

  

### Instrukcje warunkowe, for, while, ternary

Język posiada instrukcje warunkowe `if` ,`if else`, pętle `while` i `for`, oraz *ternary operator*. Każda z instrukcji wymaga bloku kodu `{}`,
ale nie wymaga nawiasów okrągłych `()`. Przykład:

```kotlin

if true {
	printInt 5;
}
// 5 (Int)
```

```kotlin
if true && false {
	printInt 5;
} else {
	printString False ? "pięć" : "nie pięć";
}
// "nie pięć" (String)
```

```kotlin
val x: Int = 8;
while x < 10 {
	printInt x;
	x++;
} 
// 8 (Int)
// 9 (Int)
```

```kotlin
val x: Int = 8;
for x from 3 to 7 {
	printInt x;
} 
// 3 (Int)
// 4 (Int)
// 5 (Int)
// 6 (Int)
// 7 (Int)
```
### Funkcje

Funkcje można deklarować na dowolnym poziomie zagnieżdżenia. Funkcje mogą zwracać wartość lub nie. Funkcje mogą przyjmować dowolną liczbę argumentów. Przykład:

```kotlin

fun aba(x: const Integer, y: const Integer) : Integer {
	return x + y;
}

fun caba(x: const Integer, y: const Integer) : Unit {
	val expr = x + y;
}

```

Zmienne widoczne są w blokach otoczonych `{` i `}`.

```kotlin
fun foo(a: Integer, b: Integer) : Integer {
	val z: Integer = 5;
	return a + b + c;
}
```

### Funkcje anonimowe

Funkcje anonimowe muszą zwracać wartość i są deklarowane w następujący sposób

```kotlin

val swap: (ref Int, ref Int ) -> Bool = (a, b) => {
	val c = a;
	a = b;
	b = c;
	return True;
}
val a = 3, b = 4;

printInt swap(a, b);
printInt a;
printInt b;
// True (Bool)
// 4
// 3
```


Lambdy mogą być przekazywane jako parametry.

```kotlin
fun IntOpApply(x: ref Int, op: (Int) -> Int) : Unit {
	x = op(x)
}
```

Typ funkcji deklarujemy używając `->` , a definicję używając ` =>`

### Załączniki
Reguły składni są w pliku `grammar/Shiro.cf`.
Tabelka w pliku `docs/tabelka-cech-imp2023.txt`
