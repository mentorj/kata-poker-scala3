package com.javaxpert.katas.scala.poker

import com.javaxpert.katas.scala.poker.Color.{HEART, TREFLE}
import com.javaxpert.katas.scala.poker.Rank.ACE

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


  @main
  def runChecks():Unit={
    val cards :List[Card] = List(Card(ACE,TREFLE),Card(ACE,HEART))
    val hand :Hand  = Hand(cards)

    println(hand.cards.groupBy(_.rank))
    println(s"Hand contains pair ? = ${containsPair(hand)}")
  }
}
