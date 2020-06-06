package altdsl

import cats.implicits._
import org.http4s.{ EntityEncoder, Method, Response, Status }
import shapeless.{ ::, HNil }

class EndpointBuilderTest {

  import cats.effect._

  case class X(m: Method, n: Method)

  val testExtractor = new Extractor.MethodMapping[IO, String](Method.GET, "tis not get!")

  val endpoint = EndpointBuilder
    .empty[IO]
    .withExtractor(testExtractor)
    .withExtractor(testExtractor)
    .consume[2, X]()
    .mapError(s => IO.pure(Response(Status.NotFound, body = EntityEncoder[IO, String].toEntity(s).body)))
    .map {
      case X(m, n) :: HNil =>
        m :: HNil
    }
    .build {
      case method :: HNil =>
        Response(Status.Ok, body = EntityEncoder[IO, String].toEntity("Got a " + method.toString).body)
    }
    .handle(IO.pure)
}
