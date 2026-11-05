# Бенчмарки

Бенчмарки оценивают влияние инлайнинга на время исполнения CBC-кода и размер полученного `default.cbc`.

Для каждого сценария подготовлены исходный вариант и вариант с ручным инлайнингом.

## Recursive tree

Этот бенчмарк оценивает эффект инлайнинга рекурсивных вызовов. Сравниваются два варианта обхода  дерева:
`visit` - стандартный рекурсивный обход
`visit_inlined` - вручную раскрыт один уровеь рекурсии: один вызов обрабатывает текущий узел и двух его детей, а рекурсия продолжается с внуков

```cangjie
func visit(tree: Tree): Int64 {
    match (tree) {
        case node: Node =>
            return 1 + visit(node.left) + visit(node.right)
        case _ =>
            return 1
    }
}
```

```cangjie
func visit_inlined(tree: Tree): Int64 {
    var result = 1
    match (tree) {
        case node: Node =>
            match (node.left) {
                case left: Node =>
                    result += 1 + visit_inlined(left.left) + visit_inlined(left.right)
                case _ => result += 1
            }
            match (node.right) {
                case right: Node =>
                    result += 1 + visit_inlined(right.left) + visit_inlined(right.right)
                case _ => result += 1
            }
        case _ => return 1
    }
    return result
}
```

### Time

No inline: 8419 ms

Inline: 6799.6 ms

### Code size

No inline: 54006 bytes

Inline: 54158 bytes


## Arrays

Тест разделяет эффект от инлайнинга двух уровней:

1. `my_abs` вызывается для каждого элемента массива;
2. `compute` вызывается один раз на внешний цикл.

`my_abs` — маленькая функция, поэтому стоимость вызова сопоставима с её полезной работой. `compute`, напротив, содержит цикл по 100 элементам.

Вариант с инлайнингом обеих функций проверяет, даёт ли удаление вызова большей функции дополнительный эффект.

```cangjie
func my_abs(x: Int64): Int64 {
    if (x < 0) { return -x }
    return x
}

func compute(...): Unit {
    for (i in 0..n) {
        let sum = a[i] + b[i] + c[i] + d[i]
        out[i] = my_abs(sum)
    }
}

func main() {
    for (_ in 0..iterations) {
        compute(a, b, c, d, out, arraySize)
    }
}
```

### Time

No inline: 5014.7 ms

Inline (`my_abs`): 4744.3 ms

Inline (`my_abs` + `compute`): 4250.4 ms

### Code size

No inline: 57822 bytes

Inline (`my_abs`): 57746 bytes

Inline (`my_abs` + `compute`): 57530 bytes

## Large method: complex fast path

`complexFastPath` содержит две ветви. В горячей ветви на каждой итерации выполняется цепочка зависимых арифметических операций с несколькими временными значениями. Холодная ветвь значительно увеличивает функцию и создаёт давление на регистры, но вызов всегда получает `flag = true`. После инлайнинга значение `flag` становится локально известным, что позволяет удалить всю холодную ветвь.

```cangjie
func complexFastPath(data: Array<Int64>, len: Int64, flag: Bool): Int64 {
    var result: Int64 = 0
    if (flag) {
        for (i in 0..len) {
            let x = data[i]
            let t1 = (x * 3 + 7) % modulus
            let t2 = (t1 * 5 + x + 11) % modulus
            let t3 = (t2 * 7 - t1 + modulus) % modulus
            let t4 = (t3 + result * 3 + i) % modulus
            result = (t4 * 11 + t2) % modulus
        }
    } else {
        // cold code
    }
    return result
}

for (iteration in 0..iterations) {
    data[0] = iteration % 1000
    complexFastPath(data, dataSize, true)
}
```

### Time

No inline: 9068.1 ms

Inline: 8671.5 ms

### Code size

No inline: 56774 bytes

Inline: 56062 bytes

## Large method: simple fast path

Структура функции такая же как в `complexFastPath`: `flag` всегда равен `true`, а холодная ветвь содержит несколько одновременно живых аккумуляторов. Отличие в горячем пути: он короткий, линейный и использует только один аккумулятор.

Этот сценарий проверяет более выгодный случай инлайнинга крупной функции. После раскрытия остаётся компактный цикл, значение `add` становится константой, а условие и большая холодная ветвь удаляются.

```cangjie
func simpleFastPath(add: Int64, data: Array<Int64>, len: Int64, flag: Bool): Int64 {
    var result: Int64 = 0
    if (flag) {
        var i: Int64 = 0
        while (i < len) {
            result += data[i] + add
            i++
        }
    } else {
        // cold code
    }
    return result
}

func main() {
    simpleFastPath(7, data, dataSize, true)
}
```

### Time

No inline: 5876.1 ms

Inline: 5131.7 ms

### Code size

No inline: 56454 bytes

Inline: 55882 bytes

## Вывод

При инлайнинге:
- сокращается число частых вызовов;
- становятся доступны специализация по константам и удаление недостижимого кода.
