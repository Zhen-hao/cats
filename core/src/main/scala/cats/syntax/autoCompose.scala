package cats
package syntax

import cats.data.AutoCompose.{AutoApply, _}

import scala.language.higherKinds

trait AutoCompositionSyntax{

    implicit class AutoFunctorSyntax[X](self: X) {
        def autoMap[A, B](f: A => B)(implicit F: AutoFunctor[X, A]): F.ComposedFunctor[B] = F.map(self)(f)
    }

    /*
    type Aux[X, A, B, CA[_]] = AutoApply2[X, A, B] { type ComposedApply[_] = CA[_]  }

    implicit class AutoApplySyntax[X, Y](first: X, second: Y) {


        def autoAp[A, B, CA[_]](ff: CA[A => B])(fa: X)(implicit aux: Aux[X, A, CA]): CA[B]
        = aux.ap[B](ff)(fa)

    }
    */


    type Aux[CA[_], X, A] = AutoApply[X, A] {
        type ComposedApply[_] = CA[_]
    }

    implicit class AutoApplySyntax[X](self: X) {

        def autoAp[CA[_], A, B](ff: CA[A => B])(implicit aux: Aux[CA, X, A]): CA[_]
        = aux.ap[B](ff)(self)

        def autoProductWith[CA[_], A, B](fb: CA[B])(implicit aux: Aux[CA, X, A]): CA[_]
        = aux.product(self, fb)

        def autoMap2With[CA[_], A, B, Z](f: (A, B) => Z)(fb: CA[B])(implicit aux: Aux[CA, X, A]): CA[_]
        = aux.map2(self, fb)(f)

    }

    case class AutoApplyPair[X, Y](x: X, y: Y){
        // def map2[A, B, Z](f: (A, B) => Z)(implicit apply: AutoApply[X, A]):
    }

}
