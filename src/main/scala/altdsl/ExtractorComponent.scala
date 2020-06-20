package altdsl

import org.http4s.{ EntityDecoder, Header }
import org.http4s.util.CaseInsensitiveString

sealed abstract class ExtractorComponent[+F[_], +E, Out] {
  def ifEmpty: () => E
}

object ExtractorComponent {
  type Pure[A] <: Nothing

  private[altdsl] case class HeaderExtractor[E, A](
    headerName: CaseInsensitiveString,
    mapfn: Header => A,
    ifEmpty: () => E)
    extends ExtractorComponent[Pure, E, A]

  private[altdsl] case class BodyExtractor[F[_], E, A, B](body: EntityDecoder[F, A], mapfn: A => B, ifEmpty: () => E)
    extends ExtractorComponent[F, E, B]

}
