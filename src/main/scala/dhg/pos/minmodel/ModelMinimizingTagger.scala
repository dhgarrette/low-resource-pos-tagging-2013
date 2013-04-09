package dhg.pos.minmodel

import dhg.util.CollectionUtil._
import dhg.nlp.util.CollectionUtils._

trait ModelMinimizingTagger[Sym, Tag] {

  /**
   * Tag each sequence using this model.
   *
   * @param rawSequences	unlabeled data to be tagged
   * @return				sequences tagged by the model
   */
  final def tag(rawTokenTagDists: Vector[Vector[(Sym, Map[Tag, Double])]]): Vector[Vector[(Sym, Tag)]] =
    (rawTokenTagDists zipSafe tagAll(rawTokenTagDists)).mapt((ws, tagged) =>
      tagged.getOrElse(throw new RuntimeException("could not tag sentence: '%s'".format(ws.map(_._1).mkString(" ")))))

  /**
   * Tag each sequence using this model.
   *
   * @param rawSequences	unlabeled data to be tagged
   * @return				sequences tagged by the model
   */
  final def tagAll(rawTokenTagDists: Vector[Vector[(Sym, Map[Tag, Double])]]): Vector[Option[Vector[(Sym, Tag)]]] =
    (rawTokenTagDists zipSafe tags(rawTokenTagDists)).map { case (ws, ts) => ts.map(ws.map(_._1) zipSafe _) }

  /**
   * Tag each sequence using this model.
   *
   * @param rawSequences	unlabeled data to be tagged
   * @return				sequences of tags returned from the model
   */
  protected def tags(rawTokenTagDists: Vector[Vector[(Sym, Map[Tag, Double])]]): Vector[Option[Vector[Tag]]]

}
