import scala.io.StdIn.readInt
//import java.io.FileReader
//import java.io.FileNotFoundException
//import java.io.IOException 
import java.lang.AssertionError
import scala.collection.immutable.List

object functionanddata {
  
  def main(arg: Array[String]){
    val x = new Rational(3,4)
    val y = new Rational(5,8)
    val z = new Rational(2,7)
    println("Value of x-y-z = " + x.sub(y).sub(z).toString())
    
    val acc1 = new Account(1.toString(),1,10000)
    val acc2 = new Account(2.toString(),2,5000)
    val acc3 = new Account(3.toString(),3,-7000)
    val acc4 = new Account(4.toString(),4,4000)
    val acc5 = new Account(5.toString(),5,-1000)
//    val acc6 = new Account(6.toString(),2,5000)
//    val acc7 = new Account(7.toString(),1,10000)
//    val acc8 = new Account(8.toString(),2,5000)
//    val acc9 = new Account(9.toString(),1,10000)
//    val acc10 = new Account(10.toString(),2,5000)
//    val acc11= new Account(11.toString(),1,10000)
//    val acc12 = new Account(12.toString(),2,5000)
//    val acc13 = new Account(13.toString(),1,10000)
//    val acc14 = new Account(14.toString(),2,5000)
//    val acc15 = new Account(15.toString(),1,10000)
//    val acc16 = new Account(16.toString(),2,5000)
//    val acc17 = new Account(17.toString(),1,10000)
//    val acc18 = new Account(18.toString(),2,5000)
//    val acc19 = new Account(19.toString(),1,10000)
//    val acc20 = new Account(20.toString(),2,5000)
    
    var bank:List[Account] = List(acc1,acc2,acc3,acc4,acc5)
//    bank += (acc1,acc2,acc3,acc4,acc5)
    
//    List of Accounts with negative balances
    val overdraft=(b:List[Account])=> b.filter((x:Account) => x.balance<0)
    
//    Total of all account balances
    val balance=(b:List[Account])=> b.map(x => (x.balance,1)).reduce((x,y) => (x._1+y._1,x._2+y._2))
    
    //If balance is positive deposit interest is .05 
    //If balance is negative overdraft interest is .1
    val interest=(b:List[Account])=>b.map(x => {
      x.balance match{
        case a if a>0 => x.deposit(x.balance*0.05)
        case _ => x.deposit(x.balance*0.1)
      }
      x
    })
    
    println("Bank Accounts - " + bank)
    
    println("Overdraft Accounts - " + overdraft(bank))
    
    val total = balance(bank);
    println("Total Balance - " + total._1)
    println("Total Accounts - " + total._2)
    
    println("New Account Balance - "+ interest(bank))
    
    acc1.transfer(acc3, 5000)
    println("Account balance after transfer - " + acc1 + " " + acc3)
    
    acc2.transfer(acc4, 6000)
    println("Account balance after transfer - " + acc2 + " " + acc4)
  }
  

}

class Rational(x:Int, y:Int=1){
  require(y>0, "Denominator must be positive")
  private val g = gcd(x.abs,y)
  val numerator:Int = x/g
  val denominator:Int = y/g
  
  private def gcd(a:Int,b:Int):Int = {
    if(b==0) a else gcd(b, a%b)
  }
  
  def neg = {
    new Rational(-numerator,denominator)
  }
  
  def +(r: Rational) = {
     new Rational(numerator * r.denominator + r.numerator * denominator, denominator * r.denominator)
  }
     
  def sub(r:Rational) = {
    this+r.neg  
  }
  
  override def toString = {
    numerator + "/" + denominator
  }
}

class Account(id:String, n:Int, b:Double){
  val nic:String = id
  val accNumber:Int  = n
  var balance:Double = b
  
  def withdraw(a:Double) = 
    this.balance=this.balance-a
    
  def deposit(a:Double) = 
    this.balance=this.balance+a

  def transfer(acc:Account, amount:Double) = {
    try{
      assert(this.balance >= amount)
      this.balance -= amount
      acc.balance += amount  
    }catch{
      case e:AssertionError => println("Insufficient Account Balance.\nCannot transfer money.")
    } 
  }
  
  override def toString = {
     "["+nic+":"+accNumber +":"+ balance+"]"
  }

}


