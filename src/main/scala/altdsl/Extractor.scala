package altdsl

import cats.{ Applicative, Functor, Semigroup }
import cats.implicits._
import cats.data.Validated
import cats.data.Validated.{ Invalid, Valid }
import org.http4s.{ DecodeFailure, EntityDecoder, Media, Method, Request }
import shapeless.{ ::, HList, HNil }
import shapeless.ops.hlist.Prepend

sealed abstract class Extractor[F[_], E] { outer =>
  type Out <: HList

  def extract(req: Request[F])(implicit F: Applicative[F]): F[Validated[E, Out]]

  final def map[B <: HList](f: Out => B): Extractor.Aux[F, E, B] = new Extractor[F, E] { inner =>
    type Out = B

    override def extract(req: Request[F])(implicit F: Applicative[F]): F[Validated[E, B]] =
      outer.extract(req).map(_.map(f))
  }

  final def ++[B <: HList, Concat <: HList](
    that: Extractor.Aux[F, E, B]
  )(
    implicit p: Prepend.Aux[Out, B, Concat],
    E: Semigroup[E]
  ): Extractor.Aux[F, E, Concat] =
    new Extractor[F, E] { inner =>
      type Out = Concat

      override def extract(req: Request[F])(implicit F: Applicative[F]): F[Validated[E, Out]] =
        F.map2(outer.extract(req), that.extract(req))((thisRes, thatRes) => thisRes.map2(thatRes)(_ ++ _))
    }
}

object Extractor {
  type Aux[F[_], E, O] = Extractor[F, E] { type Out = O }

  class MethodMapping[F[_], E: Semigroup](method: Method, default: E) extends Extractor[F, E] {
    type Out = Method :: HNil

    override def extract(req: Request[F])(implicit F: Applicative[F]): F[Validated[E, Method :: HNil]] =
      if (req.method == method) F.pure(Valid(method :: HNil))
      else F.pure(Invalid(default))
  }

  class PathMapping[F[_], A <: HList, E: Semigroup](extractor: String => Validated[E, A]) extends Extractor[F, E] {
    override type Out = A

    override def extract(req: Request[F])(implicit F: Applicative[F]): F[Validated[E, A]] =
      F.pure(extractor(req.pathInfo))
  }

  class BodyMapping[F[_]: Functor, Body, A <: HList, E: Semigroup](
    f: Body => A
  )(handleError: DecodeFailure => E
  )(strict: Boolean = false
  )(
    implicit d: EntityDecoder[F, Body])
    extends Extractor[F, E] {
    override type Out = A

    override def extract(req: Request[F])(implicit F: Applicative[F]): F[Validated[E, A]] =
      d.decode(Media(req.body, req.headers), strict).bimap(handleError, f).toValidated
  }

}
