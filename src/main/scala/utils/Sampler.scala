package utils

import scala.util.Random
import scala.collection.{Map, mutable, Set}

object Sampler {
  def sampleUniform(lo: Double, hi: Double): Double = {
    Random.nextDouble() * (hi - lo) + lo
  }

  def sampleExponential(rate: Double): Double = {
    // nextDouble samples uniformly from [0, 1)
    math.log(1 - Random.nextDouble()) / (-rate)
  }

  def sampleDiscretePowerLaw(xmin: Int, alpha: Double): Int = {
    ???
  }

  def sampleDiscreteUniform(lo: Int, hi: Int, excluded: Set[Int]): Int = {
    var randInt = lo - 1
    do {
      randInt = lo + Random.nextInt(hi - lo)
    } while (excluded.contains(randInt))
    randInt
  }

  def sampleDiscreteUniform(lo: Int, hi: Int, excluded: Set[Int], sampleSize: Int)(implicit dummyImplicit: DummyImplicit): Set[Int] = {
    val sampled = mutable.Set[Int]()
    do {
      sampled.add(sampleDiscreteUniform(lo, hi, excluded))
    } while (sampled.size < sampleSize)
    sampled
  }

  // dummy implicit here exists to differentiate the two samplePMFs
  def samplePMF[T](unnormalizedPMF: Map[T, Option[Double]])(implicit dummyImplicit: DummyImplicit): T = {
    // try to materialize pmf first
    val pmf = mutable.Map[T, Double]()
    for ((k, v) <- unnormalizedPMF) {
      v match {
        case Some(value) => pmf(k) = value
        case None =>
      }
    }
    assert(pmf.nonEmpty)
    samplePMF(pmf)
  }

  def samplePMF[T](unnormalizedPMF: Map[T, Double]): T = {
    var dart = Sampler.sampleUniform(0.0, unnormalizedPMF.values.sum)
    assert(unnormalizedPMF.nonEmpty)
    for ((n, w) <- unnormalizedPMF) {
      dart -= w
      if (dart <= 0) {
        return n
      }
    }
    throw new Exception("Sampling Failed due to Abysmal Luck")
  }

  // returns True with probability p
  def sampleBernoulli(p: Double): Boolean = {
    val rand = Random.nextDouble()
    rand < p
  }
}
