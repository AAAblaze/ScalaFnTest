package pratice.p2

/**
  * 22页练习2.4
  * 实现反柯里化，与柯里化正相反。注意，因为右箭头是右结合的，A=>(B=>C)可以写为A=>B=>C
  * def uncurry[A, B, C](f: A => B => C)
  */
object P2_4
{
    def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)
}
