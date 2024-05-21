package com.javaxpert.katas.scala.poker

/**
 * Hand evaluation result in a strength (apir, brelan...) and the level of this cards combo (highest card involved).
 * Because other parts of the  application may wish to compare hands , and two hands can contain the same combo so in this case
 * we would compare the rank (e.g:ACE SQUARE is stronger than a 5 SQUARE)
 * @param strength
 * @param rank
 * @author deadbrain <jerome@javaxpert.com>
 */
case class HandEvaluation(strength:HandStrength,rank:Rank)
