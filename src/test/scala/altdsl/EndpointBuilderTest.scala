package altdsl

import altdsl.ExtractorComponent.HeaderExtractor
import altdsl.RequestExtractor.Base
import cats.implicits._
import org.http4s.util.CaseInsensitiveString
import org.http4s.{ EntityEncoder, Header, Headers, Request, Response, Status }
import shapeless.{ ::, HNil, Id }
import zio.test.Assertion._
import zio.test._

object EndpointBuilderTest extends DefaultRunnableSpec {

  override def spec = suite("EndpointBuilderTest")(
    test("Header extraction") {
      val headerName  = "asdf"
      val headerValue = "123123"
      val header      = Header(headerName, headerValue)

      val headerNameExtractor  = HeaderExtractor(CaseInsensitiveString(headerName), _.name, () => 1) :: Base[Id]
      val headerValueExtractor = HeaderExtractor(CaseInsensitiveString(headerName), _.value, () => 1) :: Base[Id]

      val endpoint = EndpointBuilder
        .empty[Id]
        .withExtractor(headerNameExtractor)
        .withExtractor(headerValueExtractor)
        .consume[2, Header.Raw]()
        .mapError(s => Response(Status.NotFound, body = EntityEncoder[Id, String].toEntity(s.toString).body))
        .map {
          case (header: Header.Raw) :: HNil =>
            header :: HNil
        }
        .build {
          case header :: HNil =>
            Response(Status.Ok, body = EntityEncoder[Id, String].toEntity(header.value).body)
        }
        .handle(identity)

      val result = endpoint.run(Request[Id](headers = Headers(header :: Nil))).value.get.bodyAsText.compile.foldMonoid

      assert(result)(equalTo(headerValue))
    }
  )

}
