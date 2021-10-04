package mf.dabi.pso.algorithm

import breeze.linalg.DenseVector
import mf.dabi.pso.algorithm.space.SearchSpace
import mf.dabi.pso.algorithm.space.SearchSpace.{Bound, Particle, Swarm}

import scala.annotation.tailrec

object ParticleSwarmOptimization {


  /**
   * Particle Swarm Optimization (PSO) algorithm
   *
   * @param eval      num. of evaluations
   * @param swarmSize size of particle swarm
   * @param space     SearchSpace where particles live in
   * @param f         fitness function
   * @param init      initial swarm
   * @param omega     momentum factor
   * @param phip      strenght of attraction factor to best particle in the one particle's history
   * @param phig      strenght of attraction factor respect all best particle in the swarm
   * @return Minimum value
   */
  def algorithm(eval: Int, swarmSize: Int, space: SearchSpace)(f: Particle => Double, init: Bound[Double], omega: Double, phip: Double, phig: Double): Particle = {

    def step(pi: Particle, gi: Particle, vi: DenseVector[Double]): (Particle, DenseVector[Double]) = {
      val (rp, rg, x): (Double, Double, Particle) = (SearchSpace.rndm01, SearchSpace.rndm01, space.rndmX)
      val vk: DenseVector[Double] = omega * vi + phip * rp * (pi - x) + phig * rg * (gi - x)
      val vkBounded: DenseVector[Double] = SearchSpace.boundParticle(vk, space.vBound)

      val xk: Particle = x + vkBounded
      val xkBounded: Particle = SearchSpace.boundParticle(xk, space.bound)
      (xkBounded, vkBounded)
    }

    @tailrec
    def aux(acc: Int)(swarm: Swarm, velSwarm: Swarm): Particle = {
      val g: Particle = swarm.reduceLeft((acc, p) => optimal(acc, p, f))
      lazy val (swarmK, velSwarmk): (Swarm, Swarm) = swarm.zip(velSwarm).map { case (particle, vel) => step(particle, g, vel) }.unzip
      lazy val swarmOpt: Swarm = swarm.zip(swarmK).map { case (p1, p2) => optimal(p1, p2, f) }
      if (acc == 0) g else aux(acc - 1)(swarmOpt, velSwarmk)
    }

    aux(eval)(space.swarm(swarmSize, init), space.swarmV(swarmSize))
  }


  def optimal(p1: Particle, p2: Particle, f: Particle => Double): Particle =
    if (f(p1) < f(p2)) p1 else p2


}
