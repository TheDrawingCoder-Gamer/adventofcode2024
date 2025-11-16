package gay.menkissing.common
package lens

import scala.quoted.*
import monocle.*

object LensMacros:
  def makeLensImpl[S: Type, A: Type]
    (using Quotes)
    (field: Macros.Field[quotes.type, S, A]): Expr[Lens[S, A]] =

    '{
      Lens[S, A]((s: S) => ${ field.get('s) })((v: A) =>
        (s: S) => ${ field.set('s, 'v) }
      )
    }
