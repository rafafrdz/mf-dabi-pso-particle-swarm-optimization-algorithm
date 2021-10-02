package mf.dabi.pso.algorithm

import mf.dabi.pso.algorithm.space.SearchSpace.Particle

object AlghFunctions {
  val sphere: Particle => Double = (x: Particle) => x.toArray.map(d => math.pow(d, 2)).sum
}
