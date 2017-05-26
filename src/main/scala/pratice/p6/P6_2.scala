package pratice.p6

object P6_2
{
    // 练习6.2
    def double(rng:RNG):(Double, RNG) =
    {
        val (n, r) = rng.nextInt
        (n.toDouble / 2 / Int.MaxValue + 0.5, r)
    }
}
