package mf.dabi.pso.algorithm.space

import breeze.linalg.DenseVector
import mf.dabi.pso.algorithm.space.SearchSpace._

import scala.collection.parallel.immutable.ParVector
import scala.util.Random

trait SearchSpace {
  val dim: Int
  lazy val bound: Bound[Double] = List.fill(dim)((0, 1))
  lazy val vBound: Bound[Double] = bound.map { case (l0, lf) => math.abs(l0 - lf) }.map(d => (-d, d))

  /** Get a random Particle bounded by space's bound */
  def rndmX: Particle = rndmP(bound: _*)

  /** Get a random velocity vector bounded by `bound` */
  def rndmVel: DenseVector[Double] = rndmV(bound: _*)

  /** Get a random Particle bounded by `bound` */
  def rndmBounded(bound: Bound[Double]): Particle = rndmP(bound: _*)

  /** Get a random swarm of size `size` and (optional) swarm bound to inicialized */
  def swarm(size: Int): Swarm = ParVector.fill(size)(rndmX)

  def swarm(size: Int, bound: Bound[Double]): Swarm = ParVector.fill(size)(rndmBounded(bound))

  /** Get a random "swarm" of velocity vectors */
  def swarmV(size: Int): Swarm = ParVector.fill(size)(rndmVel)
}

object SearchSpace {
  type Particle = DenseVector[Double]
  type Swarm = ParVector[Particle]

  type Bound[@specialized(Int, Boolean) T] = List[Limit[T]]
  type Limit[@specialized(Int, Boolean) T] = Tuple2[T, T]


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

  /** Get pi if pi is within (bouni0, boundif) else boundi0 or boundif, for all i in [0, bound lenght - 1]´ */
  def boundParticle(p: Particle, bound: Bound[Double]): Particle =
    DenseVector(p.toArray.zip(bound).map { case (pi, limit) => boundValue(pi, limit) }: _*)

  /** Get a random number between `a` and `b` */
  def rndm(a: Double, b: Double): Double = Random.between(a, b)

  /** Get a random number within [0,1] */
  def rndm01: Double = Random.between(0, 1)
}
