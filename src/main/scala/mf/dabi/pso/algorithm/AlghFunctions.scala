package mf.dabi.pso.algorithm

import mf.dabi.pso.algorithm.space.SearchSpace.Particle

object AlghFunctions {
  val sphere: Particle => Double = (x: Particle) => x.toArray.map(d => math.pow(d, 2)).sum

  private def rosenbrockF(xs: Array[Double]): Int => Double = (i: Int) => 100 * math.pow(xs(i + 1) - math.pow(xs(i), 2), 2) + math.pow(xs(i) - 1, 2)

  val rosenbrock: Particle => Double = (x: Particle) => {
    val values: Array[Double] = x.toArray
    (0 until (values.length - 1)).map(rosenbrockF(values)).sum
  }

  private def rastriginF: Double => Double = (x: Double) => math.pow(x, 2) - 10 * math.cos(2 * math.Pi * x) + 10

  val rastrigin: Particle => Double = (x: Particle) => x.toArray.map(rastriginF).sum
}
