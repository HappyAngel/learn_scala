package happyangel.learn.scala.common

/**
  * Created by xionglei on 17-7-6.
  *
  * The Scala programming language ch18
  *
  */
abstract class Simulation {
    type Action = () => Unit
    case class WorkItem(time: Int, action: Action)
    private var curtime = 0
    def currentTime: Int = curtime
    private var agenda: List[WorkItem] = List()

    private def insert(ag: List[WorkItem], item: WorkItem): List[WorkItem] = {
        if (ag.isEmpty || item.time < ag.head.time) item :: ag
        else ag.head :: insert(ag.tail, item)
    }

    def afterDelay(delay: Int)(block: => Unit) = {
        val item = WorkItem(currentTime + delay, () => block)
        agenda = insert(agenda, item)
    }

    private def next() = {
        (agenda: @unchecked) match {
            case item :: rest =>
                agenda = rest
                curtime = item.time
                item.action()
        }
    }

    def run() = {
        afterDelay(0) {
            println("*** simulation started, time = " + currentTime + " ***")
        }
        while (!agenda.isEmpty) next()
    }

}

abstract class BasicCircuitSimulation extends Simulation {
    def InverterDelay: Int
    def AndGateDelay: Int
    def OrGateDelay: Int

    class Wire {
        private var sigVal = false
        private var actions: List[Action] = List()
        def getSignal = sigVal
        def setSignal(s: Boolean) = {
            if (s != sigVal) {
                sigVal = s
                actions foreach (_())
            }
        }
        def addAction(a: Action) = {
            actions = a :: actions
            a()
        }
    }

    def inverter(input: Wire, output: Wire) = {
        def invertAction() = {
            val inputSig = input.getSignal
            afterDelay(InverterDelay) {
                output setSignal !inputSig
            }
        }
        input addAction invertAction
    }

    def andGate(a1: Wire, a2: Wire, output: Wire) = {
        def andAction() = {
            val a1Sig = a1.getSignal
            val a2Sig = a2.getSignal
            afterDelay(AndGateDelay) {
                output setSignal (a1Sig & a2Sig)
            }
        }
        a1 addAction andAction
        a2 addAction andAction
    }

    def orGate(o1: Wire, o2: Wire, output: Wire) = {
        def orAction() = {
            val o1Sig = o1.getSignal
            val o2Sig = o2.getSignal
            afterDelay(OrGateDelay) {
                output setSignal (o1Sig | o2Sig)
            }
        }
        o1 addAction orAction
        o2 addAction orAction
    }

    def probe(name: String, wire: Wire) = {
        def probeAction() = {
            println(name + " " + currentTime + " new-value = " + wire.getSignal)
        }
        wire addAction probeAction
    }
}

abstract class CircuitSimulation extends BasicCircuitSimulation {
    def halfAdder(a: Wire, b: Wire, s: Wire, c: Wire) = {
        val d, e = new Wire
        orGate(a, b, d)
        andGate(a, b, c)
        inverter(c, e)
        andGate(d, e, s)
    }

    def fullAdder(a: Wire, b: Wire, cin: Wire, sum: Wire, cout: Wire) = {
        val s, c1, c2 = new Wire
        halfAdder(a, cin, s, c1)
        halfAdder(b, s, sum, c2)
        orGate(c1, c2, cout)
    }
}

object MySimulation extends CircuitSimulation {
    override def InverterDelay: Int = 1

    override def AndGateDelay: Int = 3

    override def OrGateDelay: Int = 5
}

object Test22 extends App {
    import MySimulation._

    val input1, input2, sum, carry = new Wire
    probe("sum", sum)
    probe("carry", carry)
    halfAdder(input1, input2, sum, carry)

    input1 setSignal true
    input2 setSignal true
    run()
}