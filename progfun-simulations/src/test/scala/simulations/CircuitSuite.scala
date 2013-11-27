package simulations

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CircuitSuite extends CircuitSimulator with FunSuite {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5
  
  test("andGate example") {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run
    
    assert(out.getSignal === false, "and 1")

    in1.setSignal(true)
    run
    
    assert(out.getSignal === false, "and 2")

    in2.setSignal(true)
    run
    
    assert(out.getSignal === true, "and 3")
  }

  //
  // to complete with tests for orGate, demux, ...
  //
  test("orGate example") {
    val in1, in2, out = new Wire
    orGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run
    
    assert(out.getSignal === false, "or 1")

    in1.setSignal(true)
    run
    
    assert(out.getSignal === true, "or 2")

    in2.setSignal(true)
    run
    
    assert(out.getSignal === true, "or 3")
  }
  
  test("orGate2 example") {
    val in1, in2, out = new Wire
    orGate2(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run
    
    assert(out.getSignal === false, "or 1")

    in1.setSignal(true)
    run
    
    assert(out.getSignal === true, "or 2")

    in2.setSignal(true)
    run
    
    assert(out.getSignal === true, "or 3")
  }
  
//  test("demux 1:2") {
//    val in, c, out0, out1 = new Wire
//    
//    in.setSignal(true)
//    c.setSignal(true)
//    demux(in, List(c), List(out0, out1))
//    run
//    
//    assert(out0.getSignal === false)
//    assert(out1.getSignal === true)
//    
//    in.setSignal(true)
//    c.setSignal(false)
//    demux(in, List(c), List(out0, out1))
//    run
//    
//    assert(out0.getSignal === true)
//    assert(out1.getSignal === false)
//  }
  
  def checkOut(out: List[Wire], truePos: Int): Boolean = {
    val trueOut = out filter (_.getSignal)
    if (trueOut.size != 1) false
    else
      (out indexOf trueOut.head) == truePos 
  }
  
  test("demux 2:4 3") {
    val in, c1, c0 = new Wire
    val out = List(new Wire, new Wire, new Wire, new Wire)
    
    in.setSignal(true)
    c1.setSignal(false)
    c0.setSignal(false)
    demux(in, List(c1, c0), out)
    run
    
    assert(out(3).getSignal)
    //assert(checkOut(out, 3))
  }
  
  ignore("demux 2:4 2") {
    val in, c1, c0 = new Wire
    val out = List(new Wire, new Wire, new Wire, new Wire)
    
    in.setSignal(true)
    c1.setSignal(false)
    c0.setSignal(true)
    demux(in, List(c1, c0), out)
    run
    
    assert(checkOut(out, 2))
  }
  
  ignore("demux 2:4 1") {
    val in, c1, c0 = new Wire
    val out = List(new Wire, new Wire, new Wire, new Wire)
    
    in.setSignal(true)
    c1.setSignal(true)
    c0.setSignal(false)
    demux(in, List(c1, c0), out)
    run
    
    assert(checkOut(out, 1))
  }
  
  test("demux 2:4 0") {
    val in, c1, c0 = new Wire
    val out = List(new Wire, new Wire, new Wire, new Wire)
    
    in.setSignal(true)
    c1.setSignal(true)
    c0.setSignal(true)
    demux(in, List(c1, c0), out)
    run
    
    assert(out(0).getSignal)
    //assert(checkOut(out, 0))
  }
  
  test("demux 4:16 3") {
    val in, c3, c2, c1, c0 = new Wire
    val out = List(new Wire, new Wire, new Wire, new Wire, 
        new Wire, new Wire, new Wire, new Wire, 
        new Wire, new Wire, new Wire, new Wire,
        new Wire, new Wire, new Wire, new Wire)
    
    in.setSignal(true)
    
    c3.setSignal(true)    
    c2.setSignal(true)    
    c1.setSignal(false)
    c0.setSignal(false)
    demux(in, List(c3, c2, c1, c0), out)
    run
    
    //assert(checkOut(out, 3))
    assert(out(3).getSignal)
  }
}
