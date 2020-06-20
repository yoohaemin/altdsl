package altdsl

import altdsl.ExtractorComponent.HeaderExtractor
import altdsl.RequestExtractor.Base
import cats.data.Validated.{ Invalid, Valid }
import cats.implicits._
import org.http4s._
import org.http4s.util.CaseInsensitiveString
import shapeless.{ HNil, Id }
import zio.test.Assertion._
import zio.test._

object RequestExtractorTest extends DefaultRunnableSpec {
  override def spec = suite("RequestExtractorTest ")(
    test("Heder existing") {
      val headerName = "asdf"
      val header     = Header(headerName, "123123")

      val res = (HeaderExtractor(CaseInsensitiveString("asdf"), identity, () => 1) :: Base[Id])
        .run(
          Request[Id](headers = Headers(header :: Nil))
        )
      assert(res)(equalTo(Valid(header :: HNil)))
    },
    test("Heder nonexistant") {
      val extractor1 = HeaderExtractor(CaseInsensitiveString("asdf"), identity, () => 2)
      val extractor2 = HeaderExtractor(CaseInsensitiveString("asdf"), identity, () => 3)
      val combined   = extractor1 :: extractor2 :: Base[Id]
      val res        = combined.run(Request[Id]())

      assert(res)(equalTo(Invalid(5)))
    }
  )
}
