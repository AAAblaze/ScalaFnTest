package pratice.p6

object P6_1
{
    // 练习6.1
    def nonNegativeInt(rng:RNG):(Int, RNG) =
    {
        val ret = rng.nextInt
        (ret._1 / 2 + Int.MaxValue / 2, ret._2)
    }
}
