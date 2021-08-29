import scala.io.StdIn.readInt

object caseclass{
  
  def main(args: Array[String]){
       
    val p1 = Point(3,4)
    val p2 = Point(-2,3)
    
    println("P1-->"+ p1+" P2-->"+ p2)
    println("P1 + P2 = " + (p1+p2))
    p1.move(-1, 5)
    println("New P1-->" + p1)
    println("Distence between "+p1+" and "+p2+" = "+ p1.distance(p2))
    p2.invert()
    println("Inverted P2-->" + p2)

  }
  
  case class Point(var X:Int,var Y:Int){
    
    def +(p:Point):Point = {
      new Point(this.X+p.X,this.Y+p.Y)
    }
    
    def move(dx:Int,dy:Int) = {
      this.X = X + dx
      this.Y = Y + dy
    }
    
    def distance(p:Point):Double = {
      math.sqrt(math.pow((this.X-p.X), 2)+math.pow((this.Y-p.Y), 2))
    }
    
    def invert() = {
      this.X = X + Y
      this.Y = X - Y
      this.X = X - Y
    }

  }
  
}