package cats
package data

import scala.language.higherKinds

object AutoCompose {

    trait AutoFunctor[X, A] {
        type ComposedFunctor[_]
        def map[B](x: X)(f: A => B): ComposedFunctor[B]
        def lift[B](f: A => B): X => ComposedFunctor[B] = map(_)(f)
    }

    object AutoFunctor {

        implicit def identityFunctor[A]: AutoFunctor[A, A] =
            new AutoFunctor[A, A] {
                type ComposedFunctor[x] = Id[x]
                def map[B](x: A)(f: A => B) = f(x)
            }


        implicit def recursiveCompose[F[_], X, A](implicit functorF: Functor[F], inner: AutoFunctor[X, A]): AutoFunctor[F[X], A] =
            new AutoFunctor[F[X], A] {
                type ComposedFunctor[x] = F[inner.ComposedFunctor[x]]
                def map[B](x: F[X])(f: A => B): F[inner.ComposedFunctor[B]] = functorF.map(x)(inner.lift(f))
            }

    }

    trait AutoApply[X, A] extends AutoFunctor[X, A] {

        type ComposedApply[_]

        type ComposedFunctor[x] = ComposedApply[x]

        //type CA[B] = ComposedApply[A => B]

        /**
          * Given a value and a function in the Apply context, applies the
          * function to the value.
          */
        def ap[B](ff: ComposedApply[A => B])(fa: X): ComposedApply[B]

        def product[B](fa: X, fb: ComposedApply[B]): ComposedApply[(A, B)]
        //= ap(map(fa)(a => (b: B) => (a, b)))(fb)


        /**
          * Applies the pure (binary) function f to the effectful values fa and fb.
          *
          * map2 can be seen as a binary version of [[cats.Functor]]#map.
          */
        def map2[B, Z](fa: X, fb: ComposedApply[B])(f: (A, B) => Z): ComposedApply[Z]
        //= map(product(fa, fb)) { case (a, b) => f(a, b) }

    }

    object AutoApply{

        implicit def identityApply[A]: AutoApply[A, A] =
            new AutoApply[A, A] {
                type ComposedApply[x] = Id[x]

                def map[B](x: A)(f: A => B) = f(x)

                def ap[B](ff: ComposedApply[A => B])(fa: A): ComposedApply[B]
                = ff(fa)

                def product[B](fa: A, fb: ComposedApply[B]): ComposedApply[(A, B)]
                = (fa, fb)

                def map2[B, Z](fa: A, fb: ComposedApply[B])(f: (A, B) => Z): ComposedApply[Z]
                = f(fa, fb)

            }


        implicit def recursiveCompose[F[_], X, A](implicit applyF: Apply[F], inner: AutoApply[X, A]): AutoApply[F[X], A] =
            new AutoApply[F[X], A] {
                type ComposedApply[x] = F[inner.ComposedApply[x]]

                def map[B](x: F[X])(f: A => B): F[inner.ComposedFunctor[B]] = applyF.map(x)(inner.lift(f))

                def ap[B](ff: ComposedApply[A => B])(fa:  F[X]): ComposedApply[B]
                = applyF.map2(fa, ff)((fa, ff) => inner.ap(ff)(fa))

                def product[B](fa: F[X], fb: ComposedApply[B]): ComposedApply[(A, B)]
                = applyF.map2(fa, fb)((fa, fb) => inner.product(fa, fb))

                def map2[B, Z](fa: F[X], fb: ComposedApply[B])(f: (A, B) => Z): ComposedApply[Z]
                = applyF.map2(fa, fb)((ga, gb) => inner.map2(ga, gb)(f))

            }


    }


/*

    type AutoApplyAux[CA[_], X, A] = AutoApply[X, A] {
        type ComposedApply[_] = CA[_]
    }

    trait AutoApply2[Ca[_], X, A, B]{

        /**
          * Given a value and a function in the Apply context, applies the
          * function to the value.
          */
        def ap(ff: Ca[A => B])(fa: X): Ca[_]

        def product(fa: X, fb: Ca[B]): Ca[_]

        /**
          * Applies the pure (binary) function f to the effectful values fa and fb.
          *
          * map2 can be seen as a binary version of [[cats.Functor]]#map.
          */
        def map2[Z](fa: X, fb: Ca[B])(f: (A, B) => Z): Ca[_]

    }


    object AutoApply2{
        implicit def auto[Ca[_], X, A, B](implicit F: AutoApplyAux[Ca, X, A]): AutoApply2[Ca, X, A, B]
        = new AutoApply2[Ca, X, A, B]{
            type Ca[x] = F.ComposedApply[x]
            def ap(ff: Ca[A => B])(fa: X): Ca[_] = F.ap[B](ff)(fa)
            def product(fa: X, fb: Ca[B]): Ca[_] = F.product[B](fa, fb)
            def map2[Z](fa: X, fb: Ca[B])(f: (A, B) => Z): Ca[_] = F.map2[B, Z](fa, fb)(f)
        }
    }
*/

/*

    trait AutoApplicative[X, A] {
        type ComposedApplicative[_]
        def pure[A](x: A): ComposedApplicative[A]
        def unit: ComposedApplicative[Unit] = pure(())

    }
*/

}
