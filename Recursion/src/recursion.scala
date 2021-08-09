import scala.io.StdIn.readInt

object recursion {
  def main(arg: Array [String]){
    
    println("Let's get started")
    
    var option = 1;

    while(option != 7){
      println("1 - Check a number is a prime number or not")
      println("2 - Print prime numbers less than N")
      println("3 - Get sum of natural number from 1 to N")
      println("4 - Check a number is even or odd")
      println("5 - Get sum of all even number less than N")
      println("6 - Print first N fibonacci numbers for given N")
      println("7 - Exit")
      
      print("Enter your option : ")
      option = readInt();
      
      var num = 0
      
      if(option != 7){
        print("\nEnter your Number : ")
        num = readInt()
      }
      option match{
        case 1 => println("Number "+num+ " is a Prime Number : " + prime(num) + "\n")
        case 2 => print("Prime Numbers below "+num+ " = ")
                  primeSeq(num)
                  println("\n")
        case 3 => println("Sum of Natural Numbers from 1 to "+num+" = " + sum(num) + "\n")
        case 4 => println("Number "+num+ " is an Even Number : " + isEven(num) + "\n")
        case 5 => println("Sum of Even Numbers less than "+num+" = "+evenSum(2,num) + "\n")
        case 6 => print("First "+num+" Fibonacci Numbers = ")
                  fibonacciSeq(num)
                  println("\n")
        case 7 => println("Exit")
        case _ => println("Invalid Input\n")
      }
    }
    
  }
  
  def prime(n:Int,i:Int=2):Boolean = {  
    n match{
      case x if (x<2) => return false
      case x if (x==2) => return true
      case x if (x%i==0) => return false
      case x if (i*i>x) => return true
      case _ =>
    }
    return prime(n, i + 1)
  }
  
  def primeSeq(n:Int):Unit = {
    if (n > 1){
      primeSeq(n-1)
      if(prime(n)){
        print(n+" ")
      }
    }
  }
  
  def sum(n:Int):Int = {
    if(n<2){
      return n
    }
    return n+sum(n-1)
  }
  
  def isEven(n:Int):Boolean= n match{
     case 0 => true
     case _ => isOdd(n-1)
  }
  
  def isOdd(n:Int):Boolean = !(isEven(n))
  
  def evenSum(n1:Int,n2:Int):Int = {
    if(n1>n2){
      return 0;
    }
    return n1+evenSum(n1+2,n2);
  }
  

  def fibonacciSeq(n:Int):Unit= {
    if (n > 0){
      fibonacciSeq(n-1)
      print(fibonacci(n) + " ")
    }
  }    
  
  def fibonacci(n:Int):Int = {
    if(n<2){
      return n
    }
    return fibonacci(n-1)+fibonacci(n-2)
  }
}