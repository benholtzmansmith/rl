import java.util.logging.Logger

import scala.util.Random


/**
  * Created by benjaminsmith on 8/23/17.
  */

object MultiArmedBandit {
  def main(args: Array[String]): Unit = {
    val logger = Logger.getLogger(MultiArmedBandit.getClass.getCanonicalName)
  }
}

case class Bandit(value:Int){
  private def result = Random.nextInt(1)

  def reward:Reward = {
    val reward =
      if (result > value) 1
      else -1

    Reward(reward)
  }
}
case class Reward(value:Int)