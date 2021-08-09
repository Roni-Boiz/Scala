import scala.io.StdIn.readInt
import scala.io.StdIn.readLine

import scala.io.Source
import scala.io.Source

import java.io.File
import java.io.PrintWriter

object caesarcipher{
  
  val key = 5;
  
  def main(arg: Array [String]){
    
    println("Caesar Cipher Problem\n")
    println("Let's get started")
    
    val ALPHABET = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
//    val alphabet = "abcdefghijklmnopqrstuvwxyz"
    
    var option = 1;
    
    while(option != 3){
      println("1 - Encrypt File")
      println("2 - Decrypt File")
      println("3 - Exit")
      
      print("Enter your option : ");
      option = readInt();
      
      option match{
        case 1 => 
//          print("\nInput your message : ")
//          val text = readLine()
//          val input = new PrintWriter(new File("src/inputfile.txt"))
//          input.write(text)
//          input.close()
          
          val inputfile = Source.fromFile("src/inputfile.txt","UTF-8");
          val message =  inputfile.getLines().mkString
          inputfile.close()
          
          println("\nMessage : " + message)  /////////////////////////////////////
          val encrypt = cipher(encryption,message,key,ALPHABET)
          println("Encrypted code : " + encrypt + "\n")
          
          val encryptfile = new PrintWriter(new File("src/encryptfile.txt"))
          encryptfile.write(encrypt)
          encryptfile.close()
          
        case 2 =>
//          print("\nInput your reply : ")
//          val text = readLine()
//          val input = new PrintWriter(new File("src/replyfile.txt"))
//          input.write(text)
//          input.close()
          
          val replyfile = Source.fromFile("src/replyfile.txt","UTF-8");
          val reply =  replyfile.getLines().mkString
          replyfile.close()
          
          println("\nReply : " + reply)  //////////////////////////////////////////
          val decrypt = cipher(decryption,reply,key,ALPHABET)
          println("Decrypted code : " + decrypt + "\n")
          
          val decryptfile = new PrintWriter(new File("src/decryptfile.txt"))
          decryptfile.write(decrypt)
          decryptfile.close()
          
        case 3 => println("Exit\n")
        case _ => println("Invalid Input\n")
      }
    }    
       
  }
  
  val cipher = (func:(Char,Int,String) => Char,s:String,key:Int,a:String) => s.map(func(_,key,a))
  
  val encryption = (c:Char,key:Int,a:String) => if (c.isLetter) a((a.indexOf(c.toUpper)+key)%a.size) else c
    
  val decryption = (c:Char,key:Int,a:String) => if (c.isLetter) a((a.indexOf(c.toUpper)-key+26)%a.size) else c
   
  } 
