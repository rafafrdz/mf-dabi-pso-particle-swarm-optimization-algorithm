package mf.dabi.pso.algorithm

import mf.dabi.pso.algorithm.AlghFunctions.{rastrigin, rosenbrock, sphere}
import mf.dabi.pso.algorithm.ParticleSwarmOptimization.algorithm
import mf.dabi.pso.algorithm.space.SearchSpace
import mf.dabi.pso.algorithm.space.SearchSpace.{Bound, Particle}

object PSOFunctions {

  val dim: Int = 20
  lazy val limitsSphere: Bound[Double] = List.fill(dim)((-100, 100))
  lazy val spaceSphere: SearchSpace = SearchSpace(dim, limitsSphere)
  def PSOSphere: Particle = algorithm(4000, 237, spaceSphere)(sphere, -0.2887, 0.4862, 2.5067)

  lazy val limitsRosenbrock: Bound[Double] = List.fill(dim)((-30, 30))
  lazy val spaceRosenbrock: SearchSpace = SearchSpace(dim, limitsRosenbrock)
  def PSORosenbrock: Particle = algorithm(4000, 237, spaceRosenbrock)(rosenbrock, -0.2887, 0.4862, 2.5067)

  lazy val limitsRastrigin: Bound[Double] = List.fill(dim)((-5.2, 5.2))
  lazy val spaceRastrigin: SearchSpace = SearchSpace(dim, limitsRastrigin)
  def PSORastrigin: Particle = algorithm(4000, 237, spaceRastrigin)(rastrigin, -0.2887, 0.4862, 2.5067)

  def main(args: Array[String]): Unit = {
    val p: Particle = PSORastrigin
    val s: Double = rastrigin(p)
    println(p)
    println(s)

  }

}
