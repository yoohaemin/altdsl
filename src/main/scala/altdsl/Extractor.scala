package altdsl

import cats.{ Applicative, Functor, Monoid, Semigroup }
import cats.syntax.functor._
import cats.syntax.apply._
import cats.data.Validated
import cats.data.Validated.{ Invalid, Valid }
import io.chrisdavenport.vault.Vault
import tofu.optics._
import org.http4s._
import shapeless.{ ::, HList, HNil }
import shapeless.ops.hlist.Prepend

//abstract class Extractor[F[_], E] { outer =>
//  type Out <: HList
//
//  type MethodMatcher[A] = Fold[Method, A]
//  type UriMatcher
//  type HttpVersionMatcher
//  type HeadersMatcher
//  type BodyMatcher[X[_]]
//  type AttributeMatcher
//
////  val method: Option[MethodMatcher] = None
////  val uri: Option[UriMatcher] = None
////  val httpVersion: Option[HttpVersionMatcher] = None
////  val headers: Option[HeadersMatcher] = None
////  val body: Option[BodyMatcher[F]] = None
////  val attributes: Option[AttributeMatcher] = None
//
//  def extract(req: Request[F])(implicit F: Applicative[F]): F[Validated[E, Out]]
//
//  final def map[B <: HList](f: Out => B): Extractor.Aux[F, E, B] = new Extractor[F, E] { inner =>
//    type Out = B
//
//    override def extract(req: Request[F])(implicit F: Applicative[F]): F[Validated[E, B]] =
//      outer.extract(req).map(_.map(f))
//  }
//
//  final def ++[B <: HList, Concat <: HList](
//    that: Extractor.Aux[F, E, B]
//  )(
//    implicit p: Prepend.Aux[Out, B, Concat],
//    E: Semigroup[E]
//  ): Extractor.Aux[F, E, Concat] =
//    new Extractor[F, E] { inner =>
//      type Out = Concat
//
//      override def extract(req: Request[F])(implicit F: Applicative[F]): F[Validated[E, Out]] =
//        F.map2(outer.extract(req), that.extract(req))((thisRes, thatRes) => thisRes.map2(thatRes)(_ ++ _))
//    }
//}

object Extractor {
//  type Aux[F[_], E, O] = Extractor[F, E] { type Out = O }
//
//  class MethodMapping[F[_], E: Semigroup](method: Method, default: E) extends Extractor[F, E] {
//    type Out = Method :: HNil
//
//    override def extract(req: Request[F])(implicit F: Applicative[F]): F[Validated[E, Method :: HNil]] =
//      if (req.method == method) F.pure(Valid(method :: HNil))
//      else F.pure(Invalid(default))
//  }
//
//  class PathMapping[F[_], A <: HList, E: Semigroup](extractor: String => Validated[E, A]) extends Extractor[F, E] {
//    override type Out = A
//
//    override def extract(req: Request[F])(implicit F: Applicative[F]): F[Validated[E, A]] =
//      F.pure(extractor(req.pathInfo))
//  }
//
//  class BodyMapping[F[_]: Functor, Body, A <: HList, E: Semigroup](
//    f: Body => A
//  )(handleError: DecodeFailure => E
//  )(strict: Boolean = false
//  )(
//    implicit d: EntityDecoder[F, Body])
//    extends Extractor[F, E] {
//    override type Out = A
//
//    override def extract(req: Request[F])(implicit F: Applicative[F]): F[Validated[E, A]] =
//      d.decode(Media(req.body, req.headers), strict).bimap(handleError, f).toValidated
//  }

  def foldHList[S, A, B <: HList](sa: Folded[S, A], sb: Folded[S, B]): Folded[S, A :: B] = new Folded[S, A :: B] {
    override def foldMap[M](f: A :: B => M)(s: S)(implicit ev: Monoid[M]): M = {
      val bmM = new Monoid[B => M] {
        override def empty: B => M                         = _ => ev.empty
        override def combine(x: B => M, y: B => M): B => M = b => ev.combine(x(b), y(b))
      }
      val bm = sa.foldMap(a => (b: B) => f(a :: b))(s)(bmM)
      sb.foldMap(bm)(s)
    }
  }

  def foldHList_[S, A, B](sa: Folded[S, A], sb: Folded[S, B]): Folded[S, A :: B :: HNil] =
    foldHList(sa, sb.composeIso(hlistIso))

  def hlistIso[A]: Iso[A, A :: HNil] = Iso[A, A :: HNil](_ :: HNil) { case a :: HNil => a }

  foldHList(Folded.id[Method], foldHList_(Folded.id[Method], Folded.id[Method]))

//   GET / "a" / "b" / pathExtractor / c

  case class PurePart(method: Method, uri: Uri, httpVersion: HttpVersion, headers: Headers, attributes: Vault)

}

trait Extractor[F[_], E] {
  import Extractor._
  type Out <: HList

  def extractors: Folded[Request[F], Out]
}
