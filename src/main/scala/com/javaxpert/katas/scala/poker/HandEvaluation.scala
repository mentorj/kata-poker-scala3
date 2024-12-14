package com.javaxpert.katas.scala.poker
import HandChecker._
/**
 * Hand evaluation result in a strength (apir, brelan...) and the level of this cards combo (highest card involved).
 * Because other parts of the  application may wish to compare hands , and two hands can contain the same combo so in this case
 * we would compare the rank (e.g:ACE SQUARE is stronger than a 5 SQUARE)
 * @param strength
 * @param rank
 * @author deadbrain <jerome@javaxpert.com>
 */
case class HandEvaluation(strength:HandStrength,rank:Rank)

object HandEvaluation{
  val scoringFunctions:List[Hand => Option[HandEvaluation]] = List(
    containsPair,handContainsBrelan,handContainsQuinteFlushRoyal,
    handContainsQuinte,handContainsColor,contains2Pairs,handContainsSquare,
    matchNoCombo
  )
  println(s"handEvaluation ${scoringFunctions}")
  def evaluateScoreFor(hand:Hand):Int =

    // app
    //val scoring = scoringFunctions.flatMap(_.apply(hand))
    val scoringOptionsList = scoringFunctions.map(_.apply(hand))
    // TODO show other ways to do that!!!
    val theOnlySomeScore = scoringOptionsList.filter(p => p.isDefined).map(o => o.get)(0)
    
    println(s"scoring hand =  ${hand} with value computed = ${theOnlySomeScore}")
    theOnlySomeScore match{
      case HandEvaluation(HandStrength.PAIR,_) => 20
      case HandEvaluation(HandStrength.DOUBLE_PAIR,_) => 40
      case HandEvaluation(HandStrength.BRELAN,_) => 50
      case HandEvaluation(HandStrength.SQUARRE,_) => 60
      case HandEvaluation(HandStrength.COLOR,_) => 80
      case HandEvaluation(HandStrength.QUINTE_FLUSH,_) => 100
      case HandEvaluation(HandStrength.QUINTE,_) => 70
      case HandEvaluation(HandStrength.FLUSH_ROYAL,Rank.ACE) => 200
      case _ => 1
     }

}
