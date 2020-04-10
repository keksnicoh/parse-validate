# parse-validate

this is just a toy project. in a recent scala project, we had introduced the types

```scala
trait Parse[T] {
  def parse(s: String): Either[String, T]
  ...
}

trait Validate[T] {
  def validate(s: T): Either[String, T]
}
```

in order to combine simple parsers / validators which then can be used to derive [json reads][1], [path bindable][2], etc.

for exmaple, `Parse[T]` implies `Reads[T]`:

```scala
def reads[T: Parse]: Reads[T] = Reads[T] {
  case JsString(s) => implicitly[Parse[T]].parse(s).fold(JsError(_), JsSuccess(_))
  case _           => JsError("expected.string")
}
```

it is usefull when data is read via different channels to avoid duplication in the validation implementation. Turns out that the types have instances for Functor, Semigroup etc. For example, `Validate[T]` forms a semigroup, so validators can be combined associatively

```haskell
someValidator = minLength 5 <> maxLength 10
```

i was interested in how the latter stuff would look like in haskell. the purpose of this repository is to practice haskell.

## Installation

run

```
stack ghci
```

to run tests

```
stack test
```

## Examples

### Validate Contravariant Functor

```bash
stack ghci
```

```haskell
import Data.Functor.Contravariant

data Derp = Derp { age :: Int, name :: String, flag :: Bool } deriving (Show)
:{
derpValidator =
  contramap age (ordMin 3 <> ordMax 1337)
    <> contramap name (maxLength 32)
    <> contramap flag (fromPredicate id)
:}
validate derpValidator (Derp 4 "peter schmidt" True)
-- Right (Derp {age = 4, name = "peter schmidt", flag = True})
validate derpValidator (Derp 1 "peter schmidt" True)
-- Left "validate.min"
validate derpValidator (Derp 3 "peter schmidt" False)
-- Left "validate.predicate"
```

[1]: https://www.playframework.com/documentation/2.8.x/ScalaJsonCombinators
[2]: https://www.playframework.com/documentation/2.8.x/ScalaRequestBinders

