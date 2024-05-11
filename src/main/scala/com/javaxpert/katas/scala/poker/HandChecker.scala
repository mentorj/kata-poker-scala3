package com.javaxpert.katas.scala.poker


object HandChecker {


  def handConformsToCriteria(hand: Hand)(desiredInstances: Int)(f: Card => Selectable)(patternForSelector: Int)(cardsPostFilterPredicate: (List[Card]) => Boolean): Boolean =
    hand.cards
      .sortWith(sortByCardRank)
      .groupBy((c) => f.apply(c))
      .filter((rank, list) => list.size == patternForSelector)
      .filter((rank, list) => cardsPostFilterPredicate.apply(list))
      .size == desiredInstances

  def containsPair(hand: Hand): Boolean =
    handConformsToCriteria(hand)(1)(_.rank)(2)(_ => true)

  def contains2Pairs(hand: Hand): Boolean =
    handConformsToCriteria(hand)(2)(_.rank)(2)(_ => true)

  def handContainsBrelan(hand: Hand): Boolean =
    handConformsToCriteria(hand)(1)(_.rank)(3)(_ => true)

  def sortByCardRank(c1: Card, c2: Card) =
    c1.rank.ordinal > c2.rank.ordinal

  def handContainsColor(hand: Hand): Boolean =
    handConformsToCriteria(hand)(1)(_.color)(5)(_ => true)


  def hansIsAFull(hand: Hand): Boolean = {
    handContainsBrelan(hand) && containsPair(hand)
  }

  def handContainsSquare(hand: Hand): Boolean = {
    handConformsToCriteria(hand)(1)(_.rank)(4)(_ => true)
  }

  def handContainsQuinte(hand: Hand): Boolean = {
    val pred = (cards: List[Card]) => cards.apply(0).rank.ordinal - cards.apply(4).rank.ordinal == 4
    handConformsToCriteria(hand)(1)(_.color)(5)(pred)
  }


}
