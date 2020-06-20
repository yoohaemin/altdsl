package altdsl

import cats.{ Applicative, Apply, Semigroup }
import cats.data.Validated
import cats.implicits._
import cats.sequence._
import org.http4s.Request
import shapeless.PolyDefns.~>
import shapeless.ops.hlist._
import shapeless.ops.sized.ToHList
import shapeless.{ ::, HList, HNil, Nat, Sized, Succ, Witness }

abstract class RequestExtractor[F[_], E] { outer =>
  type L <: Nat
  type Result <: HList
  type Out <: HList
  type Extractors <: HList

  type V[X]  = F[Validated[E, X]]
  type EC[X] = ExtractorComponent[F, E, X]

  protected def sequencer: Sequencer.Aux[Out, V, Result]
  protected def rel: NatTRel[Extractors, EC, Out, V]

  private[altdsl] def extractors: Sized[List[ExtractorComponent[F, E, _]], L]

  def ::[EE >: E, A](
    component: ExtractorComponent[F, EE, A]
  )(
    implicit sn: Witness.Aux[Succ[L]],
    F: Apply[F],
    E: Semigroup[EE]
  ): RequestExtractor.Aux[
    F,
    EE,
    A :: Result,
    Succ[L],
    F[Validated[EE, A]] :: outer.Out,
    ExtractorComponent[F, EE, A] :: outer.Extractors
  ] =
    new RequestExtractor[F, EE] { inner =>
      override type Result     = A :: outer.Result
      override type V[X]       = F[Validated[EE, X]]
      override type EC[X]      = ExtractorComponent[F, EE, X]
      override type L          = Succ[outer.L]
      override type Out        = F[Validated[EE, A]] :: outer.Out
      override type Extractors = ExtractorComponent[F, EE, A] :: outer.Extractors

      override def rel: NatTRel[
        ExtractorComponent[F, EE, A] :: outer.Extractors,
        EC,
        F[Validated[EE, A]] :: outer.Out,
        V
      ] =
        NatTRel
          .hlistNatTRel1[
            A,
            EC,
            V,
            outer.Extractors,
            outer.Out
          ](
            outer.rel.asInstanceOf[NatTRel[outer.Extractors, EC, outer.Out, V]]
          )

      override def sequencer: Sequencer.Aux[Out, V, Result] =
        Sequencer.mkHConsSequencer[V, A, outer.Out, outer.Result](
          outer.sequencer.asInstanceOf[Sequencer.Aux[outer.Out, V, outer.Result]],
          F.compose[Validated[EE, *]]
        )

      override val extractors: Sized[List[ExtractorComponent[F, EE, _]], Succ[outer.L]] =
        component +: (outer.extractors: Sized[List[ExtractorComponent[F, EE, _]], outer.L])
    }

  final def run(
    req: Request[F]
  )(
    implicit
    toHList: ToHList[List[EC[_]], L],
    F: Applicative[F]
  ): F[Validated[E, Result]] =
    rel
      .map(
        new (ExtractorComponent[F, E, *] ~> λ[X => F[Validated[E, X]]]) {
          override def apply[T](f: ExtractorComponent[F, E, T]): F[Validated[E, T]] = f match {
            case ExtractorComponent.HeaderExtractor(header, mapfn, ifEmpty) =>
              req.headers.get(header).map(mapfn).toValid(ifEmpty()).pure[F]
            case ExtractorComponent.BodyExtractor(body, mapfn, ifEmpty) => ???
          }
        },
        extractors.toHList(toHList).asInstanceOf[Extractors]
      )
      .sequence(sequencer)
}

object RequestExtractor {
  type Aux[F[_], E, Res <: HList, L0 <: Nat, O <: HList, Ext <: HList] = RequestExtractor[F, E] {
    type Result     = Res
    type L          = L0
    type Out        = O
    type Extractors = Ext
  }

  private[this] implicit val whatever: Semigroup[Nothing] = (a, b) => if (scala.util.Random.nextBoolean()) a else b
  private[this] val x: Applicative[Validated[Nothing, *]] =
    cats.data.Validated.catsDataApplicativeErrorForValidated[Nothing](whatever)

  def Base[F[_]](implicit F: Applicative[F]): RequestExtractor.Aux[F, Nothing, HNil, Nat._0, HNil, HNil] =
    new RequestExtractor[F, Nothing] {
      override type L          = Nat._0
      override type Out        = HNil
      override type Extractors = HNil
      override type Result     = HNil
      override type V[X]       = F[Validated[Nothing, X]]

      override def rel: NatTRel[HNil, EC, HNil, V] =
        NatTRel.hnilNatTRel1[ExtractorComponent[F, Nothing, *], λ[X => F[Validated[Nothing, X]]]]

      override private[altdsl] val extractors: Sized[List[ExtractorComponent[F, Nothing, _]], Nat._0] =
        Sized.wrap[List[ExtractorComponent[F, Nothing, _]], Nat._0](Nil)

      override protected def sequencer: Sequencer.Aux[HNil, V, HNil] =
        Sequencer.mkHNilSequencer[λ[X => F[Validated[Nothing, X]]]](F.compose[Validated[Nothing, *]](x))
    }
}
