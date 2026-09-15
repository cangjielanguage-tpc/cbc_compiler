struct Arrays<T> {
    var array1: Array<T>
    var array2: Array<T>

    init(array1: Array<T>, array2: Array<T>) {
        this.array1 = array1
        this.array2 = array2
    }

    mut func copyToFirst(): Unit {
        array1 = array2
    }
}

main(): Int64 {
    var numbers = Arrays<Int64>([1, 2, 3], [4, 5, 6])
    println(numbers.array1[0])
    println(numbers.array1[1])
    println(numbers.array1[2])
    numbers.copyToFirst()
    println(numbers.array1[0])
    println(numbers.array1[1])
    println(numbers.array1[2])
    0
}
