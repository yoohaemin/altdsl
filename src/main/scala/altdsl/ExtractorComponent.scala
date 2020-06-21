package altdsl

import org.http4s.util.CaseInsensitiveString
import org.http4s.{EntityDecoder, Header}
import shapeless.HList

sealed abstract class ExtractorComponent[+F[_], +E, Out] {
  def ifEmpty: () => E
  type Path <: HList
}

object ExtractorComponent {
  type Pure[A] <: Nothing

  case class HeaderExtractor[E, A](headerName: CaseInsensitiveString, ifEmpty: () => E) extends ExtractorComponent[Pure, E, Header]

  case class BodyExtractor[F[_], E, A](body: EntityDecoder[F, A], ifEmpty: () => E) extends ExtractorComponent[F, E, A]

  case class PathExtractor[E, A](ifEmpty: () => E) extends ExtractorComponent[Pure, E, A]

  case class QueryExtractor[E, A](ifEmpty: () => E) extends ExtractorComponent[Pure, E, A]

  case class MethodExtractor[E, A](ifEmpty: () => E) extends ExtractorComponent[Pure, E, A]

  case class HttpVersionExtractor[E, A](ifEmpty: () => E) extends ExtractorComponent[Pure, E, A]

  case class Mapped[F[_], E, A, B](underlying: ExtractorComponent[F, E, A], fn: A => B) extends ExtractorComponent[F, E, B] {
    override def ifEmpty: () => E = underlying.ifEmpty
  }

  val x: org.http4s.Request[cats.Id] = ???

  x.uri.query.params

}
