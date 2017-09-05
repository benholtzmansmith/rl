import java.util.logging.Logger

import breeze.linalg.support.CanCreateZerosLike
import breeze.linalg.{DenseVector, NumericOps, argmax}
import breeze.math.Ring
import breeze.optimize.proximal.LinearGenerator
import breeze.optimize.{StochasticDiffFunction, StochasticGradientDescent}

import scala.annotation.tailrec
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

    val b1 = Bandit(value = .5)
    val b2 = Bandit(value = .8)
    val b3 = Bandit(value = .9)
    val b4 = Bandit(name ="pickThisOne",value = .99)
    val bs = List(b1, b2,b3, b4)

    val resultBandits = run(bs, 10000)
    resultBandits.foreach{ b =>
      logger.info(
        s"""
           |bandit name: ${b.name}
           |bandit real reward: ${b.value}
           |bandit estimated reward: ${b.estimatedValueHistory.lastOption}
         """.stripMargin)
    }

    implicit val banditOrdering:Ordering[Bandit] = new Ordering[Bandit] {
      override def compare(x: Bandit, y: Bandit): Int = {
        val comp = for {
          b1 <- x.estimatedValueHistory.lastOption
          b2 <- y.estimatedValueHistory.lastOption
        } yield {
          if (b1 < b2) -1
          else 1
        }
        comp.getOrElse(0)
      }
    }

    logger.info(s"Bandit with highest estimated reward:${resultBandits.max.name} with reward:${resultBandits.max.estimatedValueHistory.last} ")
  }

  @tailrec
  def run(bandits:Seq[Bandit], x:Int): Seq[Bandit] ={
    if (x > 0) {
      val (remainder, optPicked) = BanditOps.chooseBandit(bandits, .1, Random.nextDouble() )
      run(remainder ++ optPicked.map{ BanditOps.updateBandit( _ ) }.toSeq, x - 1)
    } else bandits
  }
}

object BanditOps {
  def chooseBandit(bandits:Seq[Bandit], epsilon:Double, randomDouble:Double) = {
    if (epsilon > randomDouble) {
      val (h, randomBandit::tail) = bandits.splitAt(Random.nextInt(bandits.length))
      (h ++ tail, Some(randomBandit))
    }
    else {
      bandits.foldLeft((Nil:List[Bandit], None:Option[Bandit])){
        case ((runningList, previousMaxBandit), nextBandit) => {
          if(previousMaxBandit.exists(_.reward > nextBandit.reward))
            (runningList :+ nextBandit, previousMaxBandit)
          else (runningList ++ previousMaxBandit.toList, Some(nextBandit))
        }
      }
    }
  }

  def updateBandit(bandit:Bandit):Bandit = {
    Bandit(
      value = bandit.value,
      estimatedValueHistory =
        //running average of rewards
        bandit.estimatedValueHistory :+
          (bandit.estimatedValueHistory :+ bandit.reward).sum / (bandit.timesPicked + 1.0),
      timesPicked = bandit.timesPicked + 1,
      name = bandit.name
    )
  }
}

case class Bandit(
                   name:String = "aBandit",
                   value:Double = Random.nextDouble(),
                   estimatedValueHistory:List[Double] = Nil,
                   timesPicked:Int = 0
                 ) {
  /**
    * Do this because if the reward is always the same,
    * then simply the bandit with the highest reward the first time is going to
    * be the most profitable bandit
    * */
  def reward = value + Random.nextDouble()
}
