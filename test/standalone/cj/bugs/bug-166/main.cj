enum Result<T, E> {
    Ok(T) | Err(E)
}
func goo<M>(m: M): Result<M, Int64> {
    return Result<M, Int64>.Ok(m)
}
main() {
}
