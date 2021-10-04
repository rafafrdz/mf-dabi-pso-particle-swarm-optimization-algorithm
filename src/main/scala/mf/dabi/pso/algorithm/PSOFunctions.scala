package mf.dabi.pso.algorithm

import mf.dabi.pso.algorithm.AlghFunctions.{rastrigin, rosenbrock, sphere}
import mf.dabi.pso.algorithm.ParticleSwarmOptimization.algorithm
import mf.dabi.pso.algorithm.space.SearchSpace
import mf.dabi.pso.algorithm.space.SearchSpace.{Bound, Particle}

object PSOFunctions {

  val dim: Int = 2

  /** With Dim = 2 => Minimum is in (0,0) */
  lazy val limitsSphere: Bound[Double] = List.fill(dim)((-100, 100))
  lazy val spaceSphere: SearchSpace = SearchSpace(dim, limitsSphere)
  lazy val initSphere: Bound[Double] = List.fill(dim)((50, 100))

  def PSOSphere: Particle = algorithm(10000, 237, spaceSphere)(sphere, initSphere, -0.2887, 0.4862, 2.5067)

  /** With Dim = 2 => Minimum is in (1,1) */
  lazy val limitsRosenbrock: Bound[Double] = List.fill(dim)((-30, 30))
  lazy val spaceRosenbrock: SearchSpace = SearchSpace(dim, limitsRosenbrock)
  lazy val initRosenbrock: Bound[Double] = List.fill(dim)((15, 30))

  def PSORosenbrock: Particle = algorithm(10000, 237, spaceRosenbrock)(rosenbrock, initRosenbrock, -0.2887, 0.4862, 2.5067)

  /** With Dim = 2 => Minimum is in (0,0) */
  lazy val limitsRastrigin: Bound[Double] = List.fill(dim)((-5.12, 5.12))
  lazy val spaceRastrigin: SearchSpace = SearchSpace(dim, limitsRastrigin)
  lazy val initRastrigin: Bound[Double] = List.fill(dim)((2.56, 5.12))

  def PSORastrigin: Particle = algorithm(10000, 237, spaceRastrigin)(rastrigin, initRastrigin, -0.2887, 0.4862, 2.5067)

  def main(args: Array[String]): Unit = {
    val p: Particle = PSORastrigin
    val s: Double = rastrigin(p)
    println(p)
    println(s)

  }

}
