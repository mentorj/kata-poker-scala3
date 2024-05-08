package com.javaxpert.katas.scala.poker

import com.javaxpert.katas.scala.poker.Color.{HEART, SPADE, TREFLE}
import com.javaxpert.katas.scala.poker.Rank.{ACE, SIX}
import munit.FunSuite

class CardsCheckerSpec extends FunSuite{
  test("checker detects pair"){
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
    assert(HandChecker.containsPair(incompleteHandWith2Pairs) )
  }

  test("checker detects brelan") {
    val listWith1Pair: List[Card] = List(Card(ACE, HEART), Card(Rank.ACE, SPADE))
    val incompleteHandWithPair = Hand(listWith1Pair)
    assert(HandChecker.handContainsBrelan(incompleteHandWithPair) == false)

    val listWithBrelan: List[Card] = List(Card(ACE, TREFLE), Card(ACE, Color.SPADE), Card(ACE, HEART))
    val incompleteHandWithBrelan = Hand(listWithBrelan)
    assert(HandChecker.handContainsBrelan(incompleteHandWithBrelan))
  }

}
