package com.javaxpert.katas.scala.poker

import com.javaxpert.katas.scala.poker.HandStrength.SQUARRE
import com.javaxpert.katas.scala.poker.Rank.ACE


object HandChecker {


  def handConformsToCriteria(desiredInstances: Int)(f: Card => Selectable)(sortBy: (c1:Card,c2:Card) => Boolean) (patternForSelector: Int)(cardsPostFilterPredicate: (List[Card]) => Boolean)(hand: Hand): Boolean =
    hand.cards
      .sortWith(sortBy)
      .groupBy((c) => f.apply(c))
      .filter((rank, list) => list.size == patternForSelector)
      .filter((rank, list) => cardsPostFilterPredicate.apply(list))
      .size == desiredInstances


  def containsPair(hand: Hand): Option[HandEvaluation] =
    val has1Pair = handConformsToCriteria(1)(_.rank)(sortByCardRank)(2)(_ => true)(hand)
    has1Pair  match {
      case true => Some(HandEvaluation(HandStrength.QUINTE, Rank.ACE))
      case _ => None
    }

  def contains2Pairs(hand: Hand): Option[HandEvaluation] = {
    val has2Pairs = handConformsToCriteria(2)(_.rank)(sortByCardRank)(2)(_ => true)(hand)
    has2Pairs match {
      case true => Some(HandEvaluation(HandStrength.DOUBLE_PAIR, Rank.ACE))
      case _ => None
    }
  }

  def handContainsBrelan(hand: Hand): Option[HandEvaluation] = {
    val containsBrelan = handConformsToCriteria(1)(_.rank)(sortByCardRank)(3)(_ => true)(hand)
    containsBrelan match {
      case true => Some(HandEvaluation(HandStrength.BRELAN, Rank.ACE))
      case _ => None
    }
  }

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

  def handContainsColor(hand: Hand): Option[HandEvaluation] = {
    val containsColor = handConformsToCriteria(1)(_.color)(sortByCardRank)(5)(_ => true)(hand)
    containsColor match {
      case true => Some(HandEvaluation(HandStrength.COLOR, Rank.ACE))
      case _ => None
    }
  }


  def hansIsAFull(hand: Hand): Option[HandEvaluation]= {
    //val  containsFull  = handContainsBrelan(hand) && containsPair(hand)
    val option1  = handContainsBrelan(hand)
    val option2 = containsPair(hand)
    if(option1.isEmpty|| option2.isEmpty)
      None
    else
      Some(HandEvaluation(HandStrength.FULL,ACE))
  }

  def handContainsSquare(hand: Hand): Option[HandEvaluation] = {
    val containsSquare = handConformsToCriteria(1)(_.rank)(sortByCardRank)(4)(_ => true)(hand)
    containsSquare match {
      case true => Some(HandEvaluation(SQUARRE,Rank.ACE))
      case _ => None
    }
  }

  def handContainsQuinte(hand: Hand): Option[HandEvaluation] = {
    val pred = (cards: List[Card]) =>
      Math.abs(cards.apply(0).rank.ordinal - cards.apply(4).rank.ordinal) == 4
    val  containsQuinte = handConformsToCriteria(1)(_.color)(sortByCardRank)(5)(pred)(hand)
    containsQuinte match {
      case true => Some(HandEvaluation(HandStrength.QUINTE, Rank.ACE))
      case _ => None
    }
  }

  def handContainsQuinteFlushRoyal(hand: Hand):Option[HandEvaluation]={
    val pred = (cards: List[Card]) =>{
      println(s"cards in handContainsQuinteFlushRoyal is ${cards}")
      cards(0).rank==Rank.ACE && cards(4).rank==Rank.TEN
    }
    val filterHandWith5CardsWithSameColor = handConformsToCriteria(1)(_.color)(sortByAceGreatestRank)(5)
    //handConformsToCriteria(1)(_.color)(5)(pred)(hand)
    val containsRoyalFlush = filterHandWith5CardsWithSameColor.apply(pred)(hand)
    containsRoyalFlush match {
      case true => Some(HandEvaluation(HandStrength.FLUSH_ROYAL, Rank.ACE))
      case _ => None
    }
  }
  

}
