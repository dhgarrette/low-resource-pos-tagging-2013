package dhg.pos.lp

import dhg.util.CollectionUtil._

trait LpGraphBuilder[Sym] {

  def makeGraph(rawData: Vector[Vector[Sym]]): Iterator[(LpFeature, LpFeature, Double)]

  def weightWithConditionalProbByCounts(data: Vector[(Option[Sym], Option[Sym])]): Iterator[(Option[Sym], Map[Option[Sym], Double])] =
    data.groupByKey.iterator.mapVals(symbols => symbols.counts.normalizeValues.toMap)

  /**
   * (thing -> feature)  1 / (# things that have the feature)
   * (feature -> thing)  1 / (# things that have the feature)
   *
   * Edges between things and their features are weighted uniformly, with each
   * feature getting a (1/#things) portion of the mass.  Less frequent features
   * will push their labels more forcefully toward the things that have them
   * since they are "very indicative" of those things.
   */
  def makeEdges(thingsByFeature: Iterator[(LpFeature, Vector[LpFeature])]) = {
    thingsByFeature.flatMap {
      case (feature, things) =>
        val numThings = things.size
        val pThing = 1.0 / numThings
        things.flatMap { thing =>
          Seq(
            (thing, feature, pThing),
            (feature, thing, pThing))
        }
    }
  }

}
