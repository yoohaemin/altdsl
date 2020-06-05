package org.http4s.altdsl

import cats._
import cats.data._
import cats.implicits._
import org.http4s._
import shapeless.{Id => _, _}
import scala.language.implicitConversions

import cats.data.Validated.{Invalid, Valid}
import org.http4s.util.CaseInsensitiveString
import shapeless.ops.hlist.Prepend

object X {

  abstract class EndpointBuilder[F[_]: Applicative, E] { outer =>

    type Result <: HList

    def buildParam(req: Request[F]): F[Either[Option[E], Result]]

    final def withExtractor[B <: HList, Prepended <: HList, EE >: E](
      extractor: Extractor.Aux[F, EE, B]
    )(
      implicit p: Prepend.Aux[Result, B, Prepended],
      E: Semigroup[EE]
    ): EndpointBuilder.Aux[F, EE, Prepended] = new EndpointBuilder[F, EE] { inner =>
      override type Result = Prepended

      override def buildParam(req: Request[F]): F[Either[Option[EE], inner.Result]] =
        Applicative[F].map2(outer.buildParam(req), extractor.extract(req)) {
          case (Right(thisRes), Valid(thatRes)) => Right(thisRes ++ thatRes)
          case (Left(None), _)                  => Left(None)
          case (Left(Some(err)), Invalid(moar)) => Left(Some(Semigroup[EE].combine(err, moar)))
          case (Left(Some(err)), _)             => Left(Some(err))
          case (Right(_), Invalid(e))           => Left(Some(e))
        }
    }

    final def mapError[EE](f: E => EE): EndpointBuilder.Aux[F, EE, Result] = new EndpointBuilder[F, EE] {
      override type Result = outer.Result
      override def buildParam(req: Request[F]): F[Either[Option[EE], Result]] = outer.buildParam(req).map(_.left.map(_.map(f)))
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
  }

  object EndpointBuilder {

    type Aux[F[_], E, Res <: HList] = EndpointBuilder[F, E] { type Result = Res }

    def empty[F[_]: Applicative]: EndpointBuilder.Aux[F, Nothing, HNil] = new EndpointBuilder[F, Nothing] {
      override type Result = HNil

      override def buildParam(req: Request[F]): F[Either[Option[Nothing], HNil]] = Applicative[F].pure(Left(None))
    }

  }

  abstract class Endpoint[F[_]: Monad, Result] {

    def buildParam(req: Request[F]): F[Either[Option[Response[F]], Result]]

    final def handle(handler: Result => F[Response[F]]): HttpRoutes[F] =
      Kleisli { (req: Request[F]) =>
        OptionT(buildParam(req).flatMap { either =>
          either.fold[F[Option[Response[F]]]](Applicative[F].pure, param => handler(param).map(Some(_)))
        })
      }
  }

//  abstract class Extractor[F[_], +A] {
//    def extractedMethods: List[Method] = Nil
//
//    def extract(req: Request[F]): Either[Response[F], A]
//  }

//  class HeaderExtractor[F[_]](header: CaseInsensitiveString, ifNotExist: Response[F]) extends Extractor[F, Header] {
//    override final def extract(req: Request[F]): Either[Response[F], Header] =
//      req.headers.get(header).toRight(ifNotExist)
//  }

//  sealed abstract class ExtractorType extends Product with Serializable
//  object ExtractorType {
//    case object Method extends ExtractorType
//  }

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

}

object Y {
  import X._
  import cats.effect._

  val testExtractor = new Extractor.MethodMapping[IO, String](Method.GET, "tis not get!")


  val endpoint = EndpointBuilder
    .empty[IO]
    .withExtractor(testExtractor)
    .withExtractor(testExtractor)
    .mapError(s => IO.pure(Response(Status.NotFound, body = EntityEncoder[IO, String].toEntity(s).body)))
    .build { case method :: _ :: HNil =>
      Response(Status.Ok, body = EntityEncoder[IO, String].toEntity("Got a " + method.toString).body)
    }
    .handle(IO.pure)

  val x = HNil ++ HNil

  val y = x ++ HNil
}
