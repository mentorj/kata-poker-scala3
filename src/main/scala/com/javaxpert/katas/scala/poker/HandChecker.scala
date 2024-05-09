package com.javaxpert.katas.scala.poker

import com.javaxpert.katas.scala.poker.Color.{HEART, TREFLE}
import com.javaxpert.katas.scala.poker.Rank.ACE

import com.javaxpert.katas.scala.poker.CardOrdering
object HandChecker {

  def handContainsNumInstancesOfSameCardsByRank(hand:Hand)(numberOfInstances:Int)(patternByRank:Int):Boolean=
    hand.cards
      .groupBy(_.rank)
      .filter((rank, list) => list.size == patternByRank)
      .size == numberOfInstances

  def containsPair(hand: Hand): Boolean =
//    hand.cards
//      .groupBy(_.rank)
//      .filter((rank, list) => list.size == 2)
//      .size >= 1
      handContainsNumInstancesOfSameCardsByRank(hand)(1)(2)

  def contains2Pairs(hand: Hand):Boolean={
//    hand.cards
//      .groupBy(_.rank)
//      .filter((rank, list) => list.size == 2)
//      .size == 2
      handContainsNumInstancesOfSameCardsByRank(hand)(2)(2)
  }

  def handContainsBrelan(hand: Hand):Boolean={
//    hand.cards
//      .groupBy(_.rank)
//      .filter((rank,list) => list.size == 3)
//      .size ==1
      handContainsNumInstancesOfSameCardsByRank(hand)(1)(3)
  }

  def sortByCardRank(c1: Card, c2: Card) = {
    println("comparing %s and %s".format(c1, c2))
    c1.rank.ordinal > c2.rank.ordinal
  }

  def handContainsColor(hand: Hand):Boolean=
    hand.cards
      .sortWith(sortByCardRank)
      .groupBy(_.color)
      .filter((rank, list) => list.size == 5)
      .filter((c:Color,cards:List[Card]) =>
        {

          val delta:Int = cards(0).rank.ordinal- cards(4).rank.ordinal
          println(s"Delta for rank in the hand is ${delta}")
          delta== 4
        })
      .isEmpty==false


def hansIsAFull(hand:Hand):Boolean={
  handContainsBrelan(hand) && containsPair(hand)
}

def handContainsSquare(hand:Hand):Boolean=
  handContainsNumInstancesOfSameCardsByRank(hand)(1)(4)

  @main
  def runChecks():Unit={
    val cards :List[Card] = List(Card(ACE,TREFLE),Card(ACE,HEART))
    val hand :Hand  = Hand(cards)

    println(hand.cards.groupBy(_.rank))
    println(s"Hand contains pair ? = ${containsPair(hand)}")
  }
//
//  given Ordering[Card] with
//    extension (c: Card) def compare(c1:Card,c2:Card): Int =
//      c1.rank.ordinal.compare(c2.rank.ordinal)
}
