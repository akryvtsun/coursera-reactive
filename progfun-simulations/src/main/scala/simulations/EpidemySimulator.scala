package simulations

import math.random

class EpidemySimulator extends Simulator {

  def randomBelow(i: Int) = (random * i).toInt

  protected[simulations] object SimConfig {
    val population: Int = 300
    val roomRows: Int = 8
    val roomColumns: Int = 8
    
    // to complete: additional parameters of simulation
    val prevalenceRate = 1
    val transmissibilityRate = 0.4
    val diesRate = 0.25
    
    val incubationDelay = 6
    val deadDelay = 8 // 14-incubationDelay
    val immuneDelay = 2 // 16-(incubationDelay+deadDelay)
    val healthDelay = 2 // 18-(incubationDelay+deadDelay+immuneDelay)
  }

  import SimConfig._

 // val persons: List[Person] = List() // to complete: construct list of persons
  val persons: List[Person] = (for (i <- 0 until population) yield new Person(i)).toList
  
  abstract class Move {
    def doMove(row: Int, col: Int): (Int, Int);
  }
  
  object Up extends Move {
    def doMove(row: Int, col: Int): (Int, Int) = 
      (if (row == 0) roomRows-1 else row-1, col)
  }
  
  object Right extends Move {
    def doMove(row: Int, col: Int): (Int, Int) =
      (row, if (col == roomColumns-1) 0 else col+1)
  }
  
  object Down extends Move {
    def doMove(row: Int, col: Int): (Int, Int) =
      (if (row == roomRows-1) 0 else row+1, col)
  }
  
  object Left extends Move {
    def doMove(row: Int, col: Int): (Int, Int) =
      (row, if (col == 0) roomColumns-1 else col-1)
  }
  
  val moves: List[Move] = List(Up, Down, Left, Right);

  class Person (val id: Int) {
    var infected = id < population * prevalenceRate / 100
    var sick = false
    var immune = false
    var dead = false

    // demonstrates random number generation
    var row: Int = randomBelow(roomRows)
    var col: Int = randomBelow(roomColumns)
    
    //
    // to complete with simulation logic
    //
    def hasVisibleInfectious(pos: (Int, Int)): Boolean = 
      persons exists (p => p.row == pos._1 && p.col == pos._2 && (p.sick || p.dead))
      
    def hasInfectious(row: Int, col: Int): Boolean = 
      persons exists (p => p.row == row && p.col == col && p.infected)
      
    def startSickCycle = {
	    afterDelay(incubationDelay) {
	      sick = true
          if (!dead) {
	        afterDelay(deadDelay) {
	          if (random < diesRate)
	            dead = true
	          else  
	            afterDelay(immuneDelay) {
	              immune = true
	              sick = false
	              afterDelay(healthDelay) {
	                infected = false
	                immune = false
	              }
	            }
	        }
	      }      
      }
    }  
    
    def doMove(): Unit = {
      if (!dead) {
        // if not dead, list allowed moves
        val availMoves = moves filter (move => !hasVisibleInfectious(move.doMove(row, col)))
        if (!availMoves.isEmpty) {
          // if a move is possible, move (reassign row and col instantly)
          val move = availMoves(randomBelow(availMoves.size))
          val newPos = move.doMove(row, col)
          
          row = newPos._1;
          col = newPos._2;
          
          // check your neighbors after the move and eventually become infected
          if (!infected && !immune && hasInfectious(row, col) && (random < transmissibilityRate)) {
            infected = true
            startSickCycle
          }
        }
        // reschedule yourself
        afterDelay(randomBelow(5) + 1) {
          doMove()
        }
      }
    }
    
    if (infected)
      startSickCycle
    
    afterDelay(randomBelow(5) + 1) {
      doMove()
    }
  }
}
