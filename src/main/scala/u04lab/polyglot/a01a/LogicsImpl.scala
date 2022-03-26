package u04lab.polyglot.a01a
import Logics.*
import u04lab.polyglot.a01a.Logics.Result
import scala.util.Random
import u04lab.code.List

/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a01a/sol2/ */
class LogicsImpl(private val gridSize: Int, private val boatSize: Int) extends Logics:
  import List.*

  private val FAILURES = 5
  private val r = new Random()

  private var hit: List[Tuple] = Nil()
  private val boatRow = r.nextInt(gridSize)
  private val boatLeftCol = r.nextInt(gridSize - boatSize + 1)
  private var failures = 0

  println("x = " + this.boatLeftCol + " y = " + this.boatRow)

  def hit(row: Int, col: Int): Logics.Result =
    if (row == this.boatRow && col >= this.boatLeftCol && col < this.boatLeftCol + boatSize) {
      this.hit = Cons((row, col), this.hit)
      return if (List.length(this.hit) == this.boatSize) Result.WON
      else Result.HIT
    }
    this.failures += 1
    if (this.failures == FAILURES) Result.LOST
    else Result.MISS
