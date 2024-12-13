package com.javaxpert.katas.scala.poker

import com.javaxpert.katas.scala.poker.Color.{DIAMOND, HEART, SPADE, TREFLE}
import com.javaxpert.katas.scala.poker.HandStrength.{BRELAN, DOUBLE_PAIR, FULL, SQUARRE}
import com.javaxpert.katas.scala.poker.Rank.{ACE, EIGHT, FIVE, FOUR, KING, NINE, QUEEN, SEVEN, SIX, TEN, THREE, TWO}
import munit.FunSuite

import scala.math.Ordering.Implicits.*
class CardsCheckerSpec extends FunSuite{
  test("checker detects pair"){
    assert(HandChecker.containsPair(Hand(List.empty)).isEmpty)
    val listWithoutPair:List[Card]  = List(Card(ACE,TREFLE),Card(SIX,Color.SPADE))
    val incompleteHandWithoutPair = Hand(listWithoutPair)
    assert(HandChecker.containsPair(incompleteHandWithoutPair)==None)
  }

  test("checker detects 2 pairs"){

    val listWith2Pairs: List[Card] = List(Card(ACE, TREFLE), Card(ACE, Color.SPADE),Card(SIX,HEART),Card(SIX,TREFLE))

    assert(HandChecker.contains2Pairs(Hand(listWith2Pairs)).contains(HandEvaluation(DOUBLE_PAIR, ACE)))
  }

  test("checker detects brelan") {
    val listWith1Pair: List[Card] = List(Card(ACE, HEART), Card(Rank.ACE, SPADE))
    val incompleteHandWithPair = Hand(listWith1Pair)
    assert(HandChecker.handContainsBrelan(incompleteHandWithPair) == None)

    val listWithBrelan: List[Card] = List(Card(ACE, TREFLE), Card(ACE, Color.SPADE), Card(ACE, HEART))
    val incompleteHandWithBrelan = Hand(listWithBrelan)
    assert(HandChecker.handContainsBrelan(incompleteHandWithBrelan).contains(HandEvaluation(BRELAN, ACE)))
  }

  test("checker detects a  hand with color"){

    val listContainsColor :List[Card] = List(Card(SIX,SPADE),Card(SEVEN,SPADE),Card(EIGHT,SPADE),Card(Rank.NINE,Color.SPADE),Card(Rank.TEN,SPADE))
    val handWithColor = Hand(listContainsColor)
    assert(HandChecker.handContainsColor(handWithColor)==Some(HandEvaluation(HandStrength.COLOR,ACE)))
  }
  test("checker detects a full"){
    val listWithFull: List[Card] = List(Card(ACE, TREFLE), Card(ACE, Color.SPADE), Card(ACE, HEART),Card(SEVEN,SPADE),Card(SEVEN,DIAMOND))
    val completeHandWithFull = Hand(listWithFull)
    assert(HandChecker.handIsAFull(completeHandWithFull).contains(HandEvaluation(FULL, ACE)))

  }
  test("checker detects squares") {
    val listWithSquare: List[Card] = List(Card(ACE, TREFLE), Card(ACE, Color.SPADE), Card(ACE, HEART), Card(Rank.ACE, DIAMOND), Card(SEVEN, DIAMOND))
    val handWithSquare: Hand = Hand(listWithSquare)
    assert(HandChecker.handContainsSquare(handWithSquare)==Some(HandEvaluation(SQUARRE,ACE)))
  }

  test("checker detects quinte flush"){
    val listWith5CardsWithQuinteFlush: List[Card] = List(Card(SEVEN, SPADE), Card(SIX, SPADE), Card(EIGHT, SPADE), Card(Rank.NINE, Color.SPADE), Card(Rank.TEN, SPADE))

    assert(HandChecker.handContainsQuinte(Hand(listWith5CardsWithQuinteFlush)).contains(HandEvaluation(HandStrength.QUINTE, ACE)))
    val listWithColor :List[Card]   = List(Card(ACE,SPADE),Card(SIX,SPADE),Card(EIGHT,SPADE),Card(Rank.NINE,Color.SPADE),Card(Rank.TEN,SPADE))
    assert(HandChecker.handContainsQuinte(Hand(listWithColor)).isEmpty)

    val listWithQuinteAceLow = List(Card(FIVE, SPADE), Card(Rank.FOUR, SPADE), Card(THREE, SPADE), Card(Rank.TWO, Color.SPADE), Card(Rank.ACE, SPADE))
    assert(HandChecker.handContainsQuinte(Hand(listWithQuinteAceLow)).isDefined)

  }

  test("a hand with a pair is not a quinte"){
    val listWithPair = List(
      Card(Rank.ACE, Color.SPADE), Card(Rank.ACE, Color.TREFLE), Card(NINE, Color.SPADE),
      Card(TWO, Color.TREFLE), Card(TEN, Color.TREFLE)
    )
    val hand = Hand(listWithPair)
    assert(HandChecker.handContainsQuinte(hand).isEmpty)
    assert(HandChecker.containsPair(hand).isDefined)
//    assert(HandEvaluation.evaluateScoreFor(hand)==20)
  }

  test("a hand with a pair is scored to 20"){
    val listWithPair = List(
      Card(Rank.ACE,Color.SPADE),Card(Rank.ACE,Color.TREFLE),Card(NINE,Color.SPADE),
      Card(TWO,Color.TREFLE),Card(TEN,Color.TREFLE)
    )
    assert(HandEvaluation.evaluateScoreFor(Hand(listWithPair))==20)
  }

    test("a hand with brelan is scored to 50"){
    val listWithBrelan = List(
     Card(FOUR,Color.TREFLE),Card(Rank.FOUR,Color.DIAMOND),Card(Rank.FOUR,Color.SPADE),Card(Rank.TEN,Color.DIAMOND),Card(Rank.NINE,Color.SPADE)
    )
      val hand = Hand(listWithBrelan)
      assert(HandEvaluation.evaluateScoreFor(hand)==50)
  }
    test("a hand with four of a kind is scored to 60"){
      val listFourOfAKind = List(
        Card(Rank.ACE,Color.SPADE),
        Card(Rank.ACE,Color.TREFLE),
        Card(Rank.ACE,Color.DIAMOND),
        Card(Rank.ACE,Color.HEART),
        Card(Rank.TWO,Color.HEART)
      )
      val hand = Hand(listFourOfAKind)
      assert(HandEvaluation.evaluateScoreFor(hand)==60)
    }

    test("a hand with 2 pairs is scored to 40"){
      val listWith2Pairs = List(
        Card(Rank.ACE,Color.SPADE),
        Card(Rank.ACE,Color.TREFLE),
        Card(Rank.TEN,Color.SPADE),
        Card(Rank.TEN,Color.TREFLE),
        Card(Rank.SEVEN,Color.HEART)

      )
      val hand = Hand(listWith2Pairs)
      assert(HandEvaluation.evaluateScoreFor(hand)==40)
    }

    test("a hand with color is scored to 80"){
      val listWithColor =  List(
        Card(Rank.FOUR,Color.SPADE),
        Card(Rank.TEN,Color.SPADE),
        Card(Rank.TWO,Color.SPADE),
        Card(Rank.EIGHT,Color.SPADE),
        Card(Rank.SEVEN,Color.SPADE)
      )
      val hand = Hand(listWithColor)
      assert(HandEvaluation.evaluateScoreFor(hand)==80)
    }

  test("checker detects royal quinte flush"){

    val listWithQuinteAceHigh = List(Card(Rank.QUEEN, SPADE), Card(Rank.JACK, SPADE), Card(Rank.KING, Color.SPADE), Card(Rank.TEN, SPADE),Card(Rank.NINE,Color.SPADE))
    assert(HandChecker.handContainsQuinteFlushRoyal(Hand(listWithQuinteAceHigh)).isDefined)
  }


  test("Quinte flush should be rated to 100"){
    val listWithQuinteFlush =  List(Card(Rank.QUEEN, SPADE), Card(Rank.JACK, SPADE), Card(Rank.KING, Color.SPADE), Card(Rank.TEN, SPADE),Card(Rank.NINE,Color.SPADE))
    assert(HandEvaluation.evaluateScoreFor(Hand(listWithQuinteFlush))== 100)
  }

  test("hand evaluation for royal flush returns 200"){
    val listWithQuinteAceHigh = List(Card(ACE, SPADE), Card(Rank.QUEEN, SPADE), Card(Rank.JACK, SPADE), Card(Rank.KING, Color.SPADE), Card(Rank.TEN, SPADE))
    assert(HandEvaluation.evaluateScoreFor(Hand(listWithQuinteAceHigh))==200)
  }


  test("hand without  suite should be valuated to 1"){
    val handWithoutSuite = List(Card(Rank.TWO,SPADE),
      Card(Rank.THREE,SPADE),
      Card(Rank.NINE,HEART),
      Card(Rank.TEN,HEART),
      Card(Rank.KING,TREFLE)
    )
    assert(HandEvaluation.evaluateScoreFor(Hand(handWithoutSuite))==1)
  }


}
