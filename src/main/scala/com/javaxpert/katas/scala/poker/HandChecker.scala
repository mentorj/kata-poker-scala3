package com.javaxpert.katas.scala.poker

import com.javaxpert.katas.scala.poker.Color.{HEART, TREFLE}
import com.javaxpert.katas.scala.poker.Rank.ACE

object HandChecker {
  def containsPair(hand: Hand): Boolean =
    hand.cards
      .groupBy(_.rank)
      .filter(_._2.isEmpty == false)
      .filter((rank, list) => list.size == 2)
      .size >= 1


  @main
  def runChecks():Unit={
    val cards :List[Card] = List(Card(ACE,TREFLE),Card(ACE,HEART))
    val hand :Hand  = Hand(cards)

    println(hand.cards.groupBy(_.rank))
    println(s"Hand contains pair ? = ${containsPair(hand)}")
  }
}
