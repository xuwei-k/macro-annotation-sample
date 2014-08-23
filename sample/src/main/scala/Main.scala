import macroz.create

@create abstract class FoldableOps[F[_], A](self: F[A])(implicit F: scalaz.Foldable[F])

//@create class MonadOps

//@create class FunctorOps

object Main {
}
