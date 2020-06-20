package altdsl

import cats.{ Applicative, Monad, Semigroup }
import cats.implicits._
import cats.data.Validated.{ Invalid, Valid }
import org.http4s.{ Request, Response }
import shapeless.{ ::, Generic, HList, HNil, Nat }
import shapeless.ops.hlist.{ Drop, Prepend, Take }
import shapeless.ops.sized.ToHList
import singleton.ops.{ OpAuxNat, ToNat, XInt }

abstract class EndpointBuilder[F[_]: Applicative, E] { outer =>

  type Result <: HList

  def buildParam(req: Request[F]): F[Either[Option[E], Result]]

  final def withExtractor[B <: HList, Prepended <: HList, EE >: E](
    extractor: RequestExtractor[F, EE]
  )(
    implicit
    p: Prepend.Aux[Result, extractor.Result, Prepended],
    E: Semigroup[EE],
    toHList: ToHList[List[extractor.EC[_]], extractor.L],
  ): EndpointBuilder.Aux[F, EE, Prepended] = new EndpointBuilder[F, EE] { inner =>
    override type Result = Prepended

    override def buildParam(req: Request[F]): F[Either[Option[EE], inner.Result]] =
      Applicative[F].map2(outer.buildParam(req), extractor.run(req)) {
        case (Right(thisRes), Valid(thatRes)) => Right(thisRes ++ thatRes)
        case (Left(None), _)                  => Left(None)
        case (Left(Some(err)), Invalid(moar)) => Left(Some(Semigroup[EE].combine(err, moar)))
        case (Left(Some(err)), _)             => Left(Some(err))
        case (Right(_), Invalid(e))           => Left(Some(e))
      }
  }

  final def mapError[EE](f: E => EE): EndpointBuilder.Aux[F, EE, Result] = new EndpointBuilder[F, EE] {
    override type Result = outer.Result
    override def buildParam(req: Request[F]): F[Either[Option[EE], Result]] =
      outer.buildParam(req).map(_.left.map(_.map(f)))
  }

  final def build[A](builder: Result => A)(implicit F: Monad[F], ev: E <:< F[Response[F]]): Endpoint[F, A] =
    new Endpoint[F, A] { inner =>
      override def buildParam(req: Request[F]): F[Either[Option[Response[F]], A]] =
        outer.buildParam(req).flatMap { either =>
          either.bitraverse(
            _.traverse(ev),
            a => F.pure(builder(a))
          )
        }
    }

  final def map[A <: HList](f: Result => A): EndpointBuilder.Aux[F, E, A] = new EndpointBuilder[F, E] { inner =>
    override type Result = A

    override def buildParam(req: Request[F]): F[Either[Option[E], A]] =
      outer.buildParam(req).map(_.map(f))
  }

  final def consume[N <: XInt, A]: EndpointBuilder.ConsumePartiallyApplied[F, E, A, N, Result] =
    new EndpointBuilder.ConsumePartiallyApplied[F, E, A, N, Result](this)
}

object EndpointBuilder {

  class ConsumePartiallyApplied[F[_], E, A, N <: XInt, OriginalResult <: HList] private[altdsl] (
    val builder: EndpointBuilder.Aux[F, E, OriginalResult])
    extends AnyVal {

    def apply[Taken <: HList, Dropped <: HList, SNat <: Nat](
    )(
      implicit
      F: Applicative[F],
      g: Generic.Aux[A, Taken],
      nat: OpAuxNat[ToNat[N], SNat],
      take: Take.Aux[OriginalResult, SNat, Taken],
      drop: Drop.Aux[OriginalResult, SNat, Dropped],
    ): EndpointBuilder.Aux[F, E, A :: Dropped] =
      new EndpointBuilder[F, E] {
        override type Result = A :: Dropped

        override def buildParam(req: Request[F]): F[Either[Option[E], A :: Dropped]] =
          builder.buildParam(req).map(_.map(res => Generic[A].from(res.take[nat.Out]) :: res.drop[nat.Out]))
      }
  }

  type Aux[F[_], E, Res <: HList] = EndpointBuilder[F, E] { type Result = Res }

  // This will match for all requests
  def empty[F[_]: Applicative]: EndpointBuilder.Aux[F, Nothing, HNil] = new EndpointBuilder[F, Nothing] {
    override type Result = HNil

    override def buildParam(req: Request[F]): F[Either[Option[Nothing], HNil]] = Applicative[F].pure(Right(HNil))
  }

  // This will never match
  //  def passthrough[F[_]: Applicative]: EndpointBuilder.Aux[F, Nothing, Nothing] = new EndpointBuilder[F, Nothing] {
  //    override type Result = Nothing
  //
  //    override def buildParam(req: Request[F]): F[Either[Option[Nothing], Nothing]] = Applicative[F].pure(Left(None))
  //  }

}
