package com.javaxpert.katas.scala.poker

import com.javaxpert.katas.scala.poker.Color.{DIAMOND, HEART, SPADE, TREFLE}
import com.javaxpert.katas.scala.poker.Rank.{ACE, EIGHT, SEVEN, SIX}
import munit.FunSuite

class CardsCheckerSpec extends FunSuite{
  test("checker detects pair"){
    assert(HandChecker.containsPair(Hand(List.empty))==false)

    val listWith1Pair:List[Card] = List(Card(ACE,HEART),Card(Rank.ACE,SPADE))
    val incompleteHandWithPair = Hand(listWith1Pair)
    assert(HandChecker.containsPair(incompleteHandWithPair))

    val listWithoutPair:List[Card]  = List(Card(ACE,TREFLE),Card(SIX,Color.SPADE))
    val incompleteHandWithoutPair = Hand(listWithoutPair)
    assert(HandChecker.containsPair(incompleteHandWithoutPair)==false)
  }

  test("checker detects 2 pairs"){
    val listWith1Pair: List[Card] = List(Card(ACE, HEART), Card(Rank.ACE, SPADE))
    val incompleteHandWithPair = Hand(listWith1Pair)
    assert(HandChecker.contains2Pairs(incompleteHandWithPair)==false)

    val listWith2Pairs: List[Card] = List(Card(ACE, TREFLE), Card(ACE, Color.SPADE),Card(SIX,HEART),Card(SIX,TREFLE))
    val incompleteHandWith2Pairs = Hand(listWith2Pairs)
    assert(HandChecker.contains2Pairs(incompleteHandWith2Pairs) )
  }

  test("checker detects brelan") {
    val listWith1Pair: List[Card] = List(Card(ACE, HEART), Card(Rank.ACE, SPADE))
    val incompleteHandWithPair = Hand(listWith1Pair)
    assert(HandChecker.handContainsBrelan(incompleteHandWithPair) == false)

    val listWithBrelan: List[Card] = List(Card(ACE, TREFLE), Card(ACE, Color.SPADE), Card(ACE, HEART))
    val incompleteHandWithBrelan = Hand(listWithBrelan)
    assert(HandChecker.handContainsBrelan(incompleteHandWithBrelan))
  }

  test("checker detects a  hand with color"){
    val listWith5CardsWithSameColor :List[Card] = List(Card(ACE,SPADE),Card(SEVEN,SPADE),Card(EIGHT,SPADE),Card(Rank.NINE,Color.SPADE),Card(Rank.TEN,SPADE))
    val handDoesNotContainColor = Hand(listWith5CardsWithSameColor)
    assert(HandChecker.handContainsColor(handDoesNotContainColor)==false)

    val listContainsColor :List[Card] = List(Card(SIX,SPADE),Card(SEVEN,SPADE),Card(EIGHT,SPADE),Card(Rank.NINE,Color.SPADE),Card(Rank.TEN,SPADE))
    val handWithColor = Hand(listContainsColor)
    assert(HandChecker.handContainsColor(handWithColor))
  }
  test("checker detects a full"){
    val listWithFull: List[Card] = List(Card(ACE, TREFLE), Card(ACE, Color.SPADE), Card(ACE, HEART),Card(SEVEN,SPADE),Card(SEVEN,DIAMOND))
    val completeHandWithFull = Hand(listWithFull)
    assert(HandChecker.hansIsAFull(completeHandWithFull))

  }

}
