import java.util.logging.Logger

import breeze.linalg.{DenseVector, argmax}
import breeze.optimize.proximal.LinearGenerator
import breeze.optimize.{StochasticDiffFunction, StochasticGradientDescent}

import scala.util.Random


/**
  * Created by benjaminsmith on 8/23/17.
  *
  * Design a reinforcement learning algo to pick the bandit that returns the most reward.
  *
  */

object MultiArmedBandit {

  def main(args: Array[String]): Unit = {
    val logger = Logger.getLogger(MultiArmedBandit.getClass.getCanonicalName)

    val w1 = Random.nextDouble()
    val w2 = Random.nextDouble()
    val randomWeights = Seq(w1, w2)

    val bandit1 = Bandit(.5)
    val bandit2 = Bandit(.8)
    val bandits = Seq(bandit1, bandit2)

    val func = StochasticGradientDescent().minimize(Cost(bandits), randomWeights)

    println(func)
  }
}

case class Cost(bandits:Seq[Bandit]) extends StochasticDiffFunction[Seq[Double]] {
  assert(bandits.size >= 2, "must have 2 or more bandits ")

  val chanceOfRandomAction = .1

  def calculate(x: Seq[Double]): (Double, Seq[Double]) = {

    val randomNum = Random.nextDouble()

    if (randomNum > chanceOfRandomAction) {
      val (weight, bandit) = x.zip(bandits).reduceLeft{ case (c1,c2) =>
        if (c1._1 > c2._1) c1
        else c2
      }

      val loss = -1 * math.log(weight) * bandit.reward.value.toDouble
      (loss, x)
    }
    else {
      val randomIndex = Random.nextInt(bandits.length)
      val (bb, randomBandit::eb) = bandits.splitAt(randomIndex)
      val (bw, randomWeight::ew) = x.splitAt(randomIndex)
      val loss = -1 * math.log(randomWeight) * randomBandit.reward.value.toDouble
      (loss, x)
    }
  }
}

case class Bandit(value:Double){
  private def result = Random.nextInt(1)

  def reward:Reward = {
    val reward =
      if (result > value) 1
      else -1

    Reward(reward)
  }
}

case class Reward(value:Int)
