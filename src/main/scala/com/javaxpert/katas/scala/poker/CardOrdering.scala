package com.javaxpert.katas.scala.poker

implicit object CardOrdering extends Ordering[Card]{
  override def compare(c1: Card, c2: Card): Int = c1.rank.ordinal.compare(c2.rank.ordinal) 

}
