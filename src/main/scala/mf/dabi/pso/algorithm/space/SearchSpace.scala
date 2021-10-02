package mf.dabi.pso.algorithm.space

import breeze.linalg.DenseVector
import mf.dabi.pso.algorithm.space.SearchSpace._

import scala.collection.parallel.immutable.ParVector
import scala.util.Random

trait SearchSpace {
  val dim: Int
  lazy val bound: Bound[Double] = List.fill(dim)((0, 1))
  lazy val vBound: Bound[Double] = bound.map { case (l0, lf) => math.abs(l0 - lf) }.map(d => (-d, d))

  def rndmX: Particle = rndmP(bound: _*)
  def rndmVel: DenseVector[Double] = rndmV(bound: _*)

  def swarm(size: Int): Swarm = ParVector.fill(size)(rndmX)
  def swarmV(size: Int): Swarm = ParVector.fill(size)(rndmVel)
}

object SearchSpace {
  type Particle = DenseVector[Double]
  type Swarm = ParVector[Particle]

  type Bound[@specialized(Int, Boolean) T] = List[Limit[T]]
  type Limit[@specialized(Int, Boolean)T] = Tuple2[T, T]


  def apply(n: Int, limit: Bound[Double]): SearchSpace = new SearchSpace {
    override val dim: Int = n
    override lazy val bound: Bound[Double] = limit
  }

  def apply(n: Int): SearchSpace = new SearchSpace {
    override val dim: Int = n
  }

  def rndmP(bound: Limit[Double]*): Particle =
    DenseVector(bound.map { case (l0, lf) => Random.between(l0, lf) }: _*)

  def rndmV(bound: Limit[Double]*): DenseVector[Double] = {
    val nBound: Seq[Limit[Double]] = bound.map { case (l0, lf) => math.abs(l0 - lf) }.map(d => (-d, d))
    rndmP(nBound: _*)
  }

  private def boundValue(v: Double, limit: Limit[Double]): Double =
    if (v < limit._2) math.max(limit._1, v) else limit._2

  def boundParticle(p: Particle, bound: Bound[Double]): Particle =
    DenseVector(p.toArray.zip(bound).map { case (pi, limit) => boundValue(pi, limit) }: _*)

  def rndm(a: Double, b: Double): Double = Random.between(a, b)
  def rndm01: Double = Random.between(0, 1)
}
