package gay.menkissing.common

// taken from https://contributors.scala-lang.org/t/good-way-to-reuse-opaque-types-transparentness/5642
trait Newtype[Src]:
  opaque type Type = Src
  def apply(v: Src): Type = v
  protected inline def lift(v: Src): Type = v
  extension (self: Type) inline def value: Src = self
