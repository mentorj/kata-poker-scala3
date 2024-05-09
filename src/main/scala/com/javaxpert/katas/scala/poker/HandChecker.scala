package com.javaxpert.katas.scala.poker


object HandChecker {


  def handConformsToCriteria(hand:Hand)(desiredInstances:Int)(f:Card => Selectable)(patternForSelector:Int) : Boolean =
    hand.cards.groupBy((c) => f.apply(c))
      .filter((rank, list) => list.size == patternForSelector)
      .size==desiredInstances

  def containsPair(hand: Hand): Boolean =
      handConformsToCriteria(hand)(1)(_.rank)(2)

  def contains2Pairs(hand: Hand):Boolean=
    handConformsToCriteria(hand)(2)(_.rank)(2)

  def handContainsBrelan(hand: Hand):Boolean=
    handConformsToCriteria(hand)(1)(_.rank)(3)

  def sortByCardRank(c1: Card, c2: Card) =
    c1.rank.ordinal > c2.rank.ordinal

  def handContainsColor(hand: Hand):Boolean=
    handConformsToCriteria(hand)(1)(_.color)(5)


def hansIsAFull(hand:Hand):Boolean={
  handContainsBrelan(hand) && containsPair(hand)
}

def handContainsSquare(hand:Hand):Boolean=
  handConformsToCriteria(hand)(1)(_.rank)(4)

def handContainsQuinte(hand:Hand):Boolean=
  hand.cards
    .sortWith(sortByCardRank)
    .groupBy(_.color)
    .filter((rank, list) => list.size == 5)
    .filter((c:Color,cards:List[Card]) =>
       cards(0).rank.ordinal- cards(4).rank.ordinal ==  4)
    .isEmpty == false




}
