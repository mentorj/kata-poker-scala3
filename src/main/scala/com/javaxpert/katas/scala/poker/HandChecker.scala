package com.javaxpert.katas.scala.poker

import com.javaxpert.katas.scala.poker.HandStrength.{CARD_VALUE, SQUARRE}
import com.javaxpert.katas.scala.poker.Rank.ACE


/**
 * offers different functions to check poker combos in a hand
 */
object HandChecker {


  /**
   * generic HoF function used heavily by most of the other functions. Offers a great deal of flexibility with different parameters
   * @param desiredInstances, number of sets for this combo reauired by this criteeria, may 0,1 ( most frequent value),2 (for 2 pairs) 
   * @param f, how to group cards by color or rank
   * @param sortBy, how ti sort the card, useful for managing the case of ACE cards
   * @param patternForSelector, number of cards required for this combo (3 for a three of a kind, 4 for a four of kind..)
   * @param cardsPostFilterPredicate, predicate used in some cases to filter more accurately
   * @param hand, target handd to check
   * @return booleaan, true if the hand conforms to the specified criteria, false otherwise
   */
  def handConformsToCriteria(desiredInstances: Int)(f: Card => Selectable)(sortBy: (c1:Card,c2:Card) => Boolean) (patternForSelector: Int)(cardsPostFilterPredicate: (List[Card]) => Boolean)(hand: Hand): Boolean =
    hand.cards
      .sortWith(sortBy)
      .groupBy((c) => f.apply(c))
      .filter((rank, list) => list.size == patternForSelector)
      .count((rank, list) => cardsPostFilterPredicate.apply(list)) == desiredInstances


  def containsPair(hand: Hand): Option[HandEvaluation] =
    println(s"running containsPair for ${hand}")
    val has1Pair = handConformsToCriteria(1)(_.rank)(sortByCardRank)(2)(_ => true)(hand)
    println(s"has1Pair ? =${has1Pair}")
    has1Pair  match {
      case true => Some(HandEvaluation(HandStrength.PAIR, Rank.ACE))
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

  /**
   * required to process the ACE cards because these cards have 2 values
   * straight with ACE,2,3,4,5 or 10,JACK,QUEEN,KING,ACE
   * this function computes the rank  using ACE 's rank > KING's rank
   * @param c1, first card to be compared
   * @param c2, second card 
   * @return true if c1 has rank greater than the c2 rank false otherwise
   */
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

  /**
   * function used to rank hands without any specific ccmbo
   * @param hand, hand to score
   * @return None if this hand has a combo (pair, flush..), Some(HandEvaluation(HandStrength.CARD_VALUE)) if no combo found
   */
  def matchNoCombo(hand: Hand):Option[HandEvaluation] = {
    val hasRankCombo: Boolean =
      handConformsToCriteria(1)(_.rank)(sortByCardRank)(2)(_ => true)(hand) ||
      handConformsToCriteria(1)(_.rank)(sortByCardRank)(3)(_ => true)(hand) ||
      handConformsToCriteria(1)(_.rank)(sortByCardRank)(4)(_ => true)(hand) ||
      handConformsToCriteria(2)(_.rank)(sortByCardRank)(2)(_ => true)(hand)

    val hasColorCombo: Boolean = handConformsToCriteria(1)(_.color)(sortByCardRank)(5)(_ => true)(hand)

    val matchCombo = hasColorCombo || hasRankCombo
    matchCombo match {
      case true => None
      // TODO fix rank later
      case _ => Some(HandEvaluation(CARD_VALUE,Rank.ACE))
    }
  }




  def handIsAFull(hand: Hand): Option[HandEvaluation]= {
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
    println("entering handContainsQuinte...")
    val pred = (cards: List[Card]) =>
      Math.abs(cards.apply(0).rank.ordinal - cards.apply(4).rank.ordinal) == 4
    val  containsQuinte = handConformsToCriteria(1)(_.rank)(sortByCardRank)(5)(pred)(hand)
    println(s"handContainsQuinte = ${containsQuinte}")
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
