package com.javaxpert.katas.scala.poker


object HandChecker {


  def handConformsToCriteria(desiredInstances: Int)(f: Card => Selectable)(sortBy: (c1:Card,c2:Card) => Boolean) (patternForSelector: Int)(cardsPostFilterPredicate: (List[Card]) => Boolean)(hand: Hand): Boolean =
    hand.cards
      .sortWith(sortBy)
      .groupBy((c) => f.apply(c))
      .filter((rank, list) => list.size == patternForSelector)
      .filter((rank, list) => cardsPostFilterPredicate.apply(list))
      .size == desiredInstances


  def containsPair(hand: Hand): Boolean =
    handConformsToCriteria(1)(_.rank)(sortByCardRank)(2)(_ => true)(hand)

  def contains2Pairs(hand: Hand): Boolean =
    handConformsToCriteria(2)(_.rank)(sortByCardRank)(2)(_ => true)(hand)

  def handContainsBrelan(hand: Hand): Boolean =
    handConformsToCriteria(1)(_.rank)(sortByCardRank)(3)(_ => true)(hand)

  def sortByCardRank(c1: Card, c2: Card): Boolean =
    c1.rank.ordinal > c2.rank.ordinal

  def sortByAceGreatestRank(c1: Card, c2: Card) ={
    var rank1Ordinal = c1.rank.ordinal
    var rank2Ordinal = c2.rank.ordinal
    if (c1.rank == Rank.ACE) {
      rank1Ordinal = Rank.KING.ordinal + 1
    }
    if (c2.rank == Rank.ACE) {
      rank2Ordinal = Rank.KING.ordinal + 1
    }
    rank1Ordinal > rank2Ordinal
  }

  def handContainsColor(hand: Hand): Boolean =
    handConformsToCriteria(1)(_.color)(sortByCardRank)(5)(_ => true)(hand)


  def hansIsAFull(hand: Hand): Boolean = {
    handContainsBrelan(hand) && containsPair(hand)
  }

  def handContainsSquare(hand: Hand): Boolean = {
    handConformsToCriteria(1)(_.rank)(sortByCardRank)(4)(_ => true)(hand)
  }

  def handContainsQuinte(hand: Hand): Boolean = {
    val pred = (cards: List[Card]) =>
      Math.abs(cards.apply(0).rank.ordinal - cards.apply(4).rank.ordinal) == 4
    handConformsToCriteria(1)(_.color)(sortByCardRank)(5)(pred)(hand)
  }

  def handContainsQuinteFlushRoyal(hand: Hand):Boolean={
    val pred = (cards: List[Card]) =>{
      println(s"cards in handContainsQuinteFlushRoyal is ${cards}")
      cards(0).rank==Rank.ACE && cards(4).rank==Rank.TEN
    }
    val filterHandWith5CardsWithSameColor = handConformsToCriteria(1)(_.color)(sortByAceGreatestRank)(5)
    //handConformsToCriteria(1)(_.color)(5)(pred)(hand)
    filterHandWith5CardsWithSameColor.apply(pred)(hand)
  }
  

}
