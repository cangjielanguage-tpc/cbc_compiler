import hotfix.patchable

@patchable
func apply(f: (Int64) -> Int64, value: Int64): Int64 {
  f(value)
}

@patchable
func applyGeneric<T>(f: (T) -> T, value: T): T {
  f(value)
}

@patchable
func applyNested(f: () -> (Int64) -> Int64): Int64 {
  f()(42)
}
