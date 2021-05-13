object wholesaleCost extends App{

  def cost(x:Int):Double = {
    if(x<=50){
      return 24.95*x*60/100+3;
    }else{
      return 24.95*x*60/100+3+(x-50)*0.75;
    }
  }

  println("Wholesale cost for 60 copies of books = "+cost(60));

}
