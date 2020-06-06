package altdsl

import cats._
import cats.data._
import cats.implicits._
import org.http4s._

import scala.language.implicitConversions

abstract class Endpoint[F[_]: Monad, Result] {

  def buildParam(req: Request[F]): F[Either[Option[Response[F]], Result]]

  final def handle(handler: Result => F[Response[F]]): HttpRoutes[F] =
    Kleisli { (req: Request[F]) =>
      OptionT(buildParam(req).flatMap { either =>
        either.fold[F[Option[Response[F]]]](Applicative[F].pure, param => handler(param).map(Some(_)))
      })
    }
}
