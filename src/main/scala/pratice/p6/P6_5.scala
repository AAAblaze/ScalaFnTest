package pratice.p6


object P6_5
{
    type Rand[+A] = RNG => (A, RNG)

    def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng =>
    {
        val (a, rng2) = s(rng)
        (f(a), rng2)
    }

    // 练习6.5
    def double(rng: RNG): (Double, RNG) = map(P6_1.nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))(rng)

    // 练习6.6
    def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng =>
    {
        val (a, rng2) = ra(rng)
        val (b, rng3) = rb(rng2)
        (f(a, b), rng3)
    }
}
