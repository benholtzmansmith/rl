import scala.annotation.tailrec

/**
  * Created by benjaminsmith on 8/28/17.
  */
object MathHelpers {

  object IntOps {
    implicit class CountDown(i:Int){
      def countDown[A](a:A)(f:(Int, A) => A):A = {
        @tailrec
        def loop (n:Int, aa:A): A = {
          if(n == 0) aa
          else loop( n - 1, f(n, aa))
        }
        loop(i, f(i, a))
      }
    }
  }
}
