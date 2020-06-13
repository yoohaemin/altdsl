package altdsl

import cats.{ Applicative, Apply, Functor, Id, Monoid, Semigroup }
import cats.implicits._
import cats.syntax.functor._
import cats.syntax.apply._
import cats.data.Validated
import cats.data.Validated.{ Invalid, Valid }
import io.chrisdavenport.vault.Vault
import tofu.optics._
import org.http4s._
import org.http4s.util.CaseInsensitiveString
import shapeless.PolyDefns.Case
import shapeless._
import shapeless.ops.hlist.{ Mapped, Mapper, Prepend }

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

//object Extractor {
//  type Aux[F[_], E, O] = Extractor[F, E] { type Out = O }
////
////  class MethodMapping[F[_], E: Semigroup](method: Method, default: E) extends Extractor[F, E] {
////    type Out = Method :: HNil
////
////    override def extract(req: Request[F])(implicit F: Applicative[F]): F[Validated[E, Method :: HNil]] =
////      if (req.method == method) F.pure(Valid(method :: HNil))
////      else F.pure(Invalid(default))
////  }
////
////  class PathMapping[F[_], A <: HList, E: Semigroup](extractor: String => Validated[E, A]) extends Extractor[F, E] {
////    override type Out = A
////
////    override def extract(req: Request[F])(implicit F: Applicative[F]): F[Validated[E, A]] =
////      F.pure(extractor(req.pathInfo))
////  }
////
////  class BodyMapping[F[_]: Functor, Body, A <: HList, E: Semigroup](
////    f: Body => A
////  )(handleError: DecodeFailure => E
////  )(strict: Boolean = false
////  )(
////    implicit d: EntityDecoder[F, Body])
////    extends Extractor[F, E] {
////    override type Out = A
////
////    override def extract(req: Request[F])(implicit F: Applicative[F]): F[Validated[E, A]] =
////      d.decode(Media(req.body, req.headers), strict).bimap(handleError, f).toValidated
////  }
//
//  def foldHList[S, A, B <: HList](sa: Folded[S, A], sb: Folded[S, B]): Folded[S, A :: B] = new Folded[S, A :: B] {
//    override def foldMap[M](s: S)(f: A :: B => M)(implicit ev: Monoid[M]): M = {
//      val bmM = new Monoid[B => M] {
//        override def empty: B => M                         = _ => ev.empty
//        override def combine(x: B => M, y: B => M): B => M = b => ev.combine(x(b), y(b))
//      }
//      val bm = sa.foldMap(s)(a => (b: B) => f(a :: b))(bmM)
//      sb.foldMap(s)(bm)
//    }
//  }
//
//  def foldHList_[S, A, B](sa: Folded[S, A], sb: Folded[S, B]): Folded[S, A :: B :: HNil] =
//    foldHList(sa, PFolded.compose(hlistEquivalent, sb))
//
//  def hlistEquivalent[A]: Equivalent[A, A :: HNil] =
//    Equivalent[A][A :: HNil](_ :: HNil) { case a :: HNil => a }
//
////   GET / "a" / "b" / pathExtractor / c
//}

trait Extractor[F[_], E, A] extends Downcast[Request[F], A] { outer =>
  def downcast(s: Request[F]): Option[A]
  def error: Option[E]
  final def extract(s: Request[F]): Validated[Option[E], A] = downcast(s) match {
    case None        => Invalid(error)
    case Some(value) => Valid(value)
  }

  def +[B <: HList, C](
    that: Extractor[F, E, B]
  )(
    implicit ev1: A <:!< HList,
    ev2: B <:< HList,
    E: Semigroup[E]
  ): MultipleExtractor[F, E, A :: B] = new MultipleExtractor[F, E, A :: B] { inner =>
    override def downcast(s: Request[F]): Option[A :: B] =
      Apply[Option].map2(outer.downcast(s), that.downcast(s))(_ :: _)
    override def error: Option[E] = outer.error |+| that.error
  }

  def +[B, C](
    that: Extractor[F, E, B]
  )(
    implicit ev1: A <:!< HList,
    ev2: B <:!< HList,
    E: Semigroup[E]
  ): MultipleExtractor[F, E, A :: B :: HNil] = new MultipleExtractor[F, E, A :: B :: HNil] {
    override def downcast(s: Request[F]): Option[A :: B :: HNil] =
      Apply[Option].map2(outer.downcast(s), that.downcast(s))(_ :: _ :: HNil)
    override def error: Option[E] =
      outer.error |+| that.error
  }
}

trait MultipleExtractor[F[_], E, A <: HList] extends Extractor[F, E, A] { outer =>

  final def ++[B <: HList, C <: HList](
    that: Extractor[F, E, B]
  )(
    implicit ev: B <:< HList,
    prepended: Prepend.Aux[A, B, C],
    E: Semigroup[E]
  ): MultipleExtractor[F, E, C] = new MultipleExtractor[F, E, C] {
    override def downcast(req: Request[F]): Option[C] =
      Apply[Option].map2(outer.downcast(req), that.downcast(req))(prepended.apply)
    override def error: Option[E] = outer.error |+| that.error
  }

  def +:[B](that: Extractor[F, E, B])(implicit ev2: B <:!< HList, E: Semigroup[E]): MultipleExtractor[F, E, B :: A] =
    new MultipleExtractor[F, E, B :: A] {
      override def downcast(s: Request[F]): Option[B :: A] =
        Apply[Option].map2(that.downcast(s), outer.downcast(s))(_ :: _)
      override def error: Option[E] =
        outer.error |+| that.error
    }

}

object Extractor {

  implicit def toMultipleExtractor[F[_], E, A <: HList](extractor: Extractor[F, E, A]): MultipleExtractor[F, E, A] =
    new MultipleExtractor[F, E, A] {
      override def downcast(s: Request[F]): Option[A] = ???
      override def error: Option[E]                   = ???
    }

  implicit def toFExtractor[F[_], E, A](extractor: PureExtractor[E, A]): Extractor[F, E, A] = new Extractor[F, E, A] {
    override def downcast(s: Request[F]): Option[A] =
      extractor.downcast(s.asInstanceOf[Request[X forSome { type X[_] }]])
    override def error: Option[E] = extractor.error
  }

  val a: Extractor[Id, Int, Int]            = ???
  val b: Extractor[Id, Int, String :: HNil] = ???

  val c: Extractor[Id, Int, Int :: String :: HNil] = a +: b

  val d: MultipleExtractor[Id, Int, Int :: String :: Int :: String :: HNil] = (a +: b) ++ c

  val e = a + a

  import fs2.Pure

  trait PureExtractor[E, A] extends Extractor[X forSome { type X[_] }, E, A] {
    final def to[F[_]]: Extractor[F, E, A] = toFExtractor(this)
  }

  object bbb extends PureExtractor[Int, Int] {
    override def downcast(s: Request[X forSome { type X[_] }]): Option[Int] = ???
    override def error: Option[Int]                                         = ???
  }

  a + bbb.to[Id]

//  class HeaderExtractor[F[_], E, A](headerName: CaseInsensitiveString, f: Header => A, default: => Option[E]) extends Extractor[F, E, A] {
//    override def downcast(s: Request[F]): Option[A] = s.headers.get(headerName).map(f)
//    override lazy val error: Option[E] = default
//  }
  trait PathMatcher[A <: HList] {}

  trait PathExtractor[E, A <: HList] extends PureExtractor[E, A] with MultipleExtractor[X forSome { type X[_] }, E, A] {
    outer =>
    final def /(str: String): PathExtractor[E, A] = new PathExtractor[E, A] {
      override def downcast(s: Request[F forSome { type F[_] }]): Option[A] = ???
      override def error: Option[E]                                         = ???
    }
    final def / : PathExtractor[E, A]                                     = /("") // This will require feature import
    final def /[B](pathMatcher: PathMatcher[B]): PathExtractor[E, B :: A] = ???
  }

  val GET = new PathExtractor[Nothing, HNil] { outer =>
    override def downcast(s: Request[X forSome { type X[_] }]): Option[HNil] =
      if (s.method == Method.GET) Some(HNil) else None
    override def error: Option[Nothing] = None
  }
  import scala.language.postfixOps

  GET / "Hello"

  class MethodExtractor[E, A](method: Method, default: => Option[E]) extends PureExtractor[E, Method] {
    override def downcast(s: Request[X forSome { type X[_] }]): Option[Method] =
      if (s.method == method) Some(method) else None
    override lazy val error: Option[E] = default
  }

  class UriExtractor[F[_], E, A](f: Uri => Option[A], default: => Option[E]) extends Extractor[F, E, A] {
    override def downcast(s: Request[F]): Option[A] = f(s.uri)
    override lazy val error: Option[E]              = default
  }

//  val uri: Uri = Uri(path = "/"),
//  val httpVersion: HttpVersion = HttpVersion.`HTTP/1.1`,
//  val headers: Headers = Headers.empty,
//  val body: EntityBody[F] = EmptyBody,
//  val attributes: Vault = Vault.empty
}

//  import Extractor._
//  type Out <: HList
//  type Foldeds <: HList
//  type M = Mapped.Aux[Out, Folded[Request[F], *], Foldeds]
//
//  def foldeds: Foldeds

//  class f(val req: Request[F]) extends Poly {
//    implicit def foldedCase[A]: Case.Aux[f, Folded[Request[F], A], List[A]] =
//      new Case[f, Folded[Request[F], A]] {
////        type Out = List[A]
////        override def apply(x: Folded[Request[F], A]): List[A] = x.getAll(req)
//        override type Result = List[A]
//        override val value: Folded[Request[F], A] => Result = f => f.getAll(req)
//      }
//  }
//
//  def extract(req: Request[F])(implicit m: M, mapper: Mapper.Aux[f, Foldeds, Out]): Out = foldeds.map(new f(req))(mapper)
