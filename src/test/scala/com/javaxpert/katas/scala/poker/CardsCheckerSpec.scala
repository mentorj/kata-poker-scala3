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

}
