package fileSearch

import org.scalatest._
import sys.process._
import java.io.File
import java.io.FileWriter
import java.io.BufferedWriter
import SimpleFind._
import scala.util.Success
import scala.util.Failure

class SimpleFindTest extends FlatSpec with Matchers {

  def mkdir(s: String) = {
	  val exitCode = ("mkdir " +s).!
	  exitCode
  }
  
  def writeFile(fileName: String, contents: String) = {
    val file = new File(fileName)
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(contents)
    bw.close
  }
  
  def removeDir(s: String) = {
      val exitCode = ("rm -r " +s).!
	  exitCode 
  }
  
  def removeFile(s: String) = {
      val exitCode = ("rm " +s).!
	  exitCode 
  }
  

  "getListOfSubDirectories" should "list all subdirectories in a given directory" in {

       mkdir("Foo")
       writeFile("Foo/foo1.txt", "test1")
       writeFile("Foo/foo2.txt", "test2")
       
       mkdir("Foo/Bar")
       mkdir("Foo/Bar/Bar1")
       mkdir("Foo/Bar/Bar2")
       
       getListOfSubDirectories(new File("Foo")) should be 
        (List( new File("Foo"), new File("Foo/Bar"), new File("Foo/Bar/Bar1"), 
            new File("Foo/Bar/Bar2")))
       
       getListOfSubDirectories(new File("Foo/Bar/Bar2")) should be (List(new File("Foo/Bar/Bar2")))
             
       removeDir("Foo")        
  }
  
  "getListOfFiles" should "list files in a given directory" in {

       mkdir("Foo")
       writeFile("Foo/foo1.txt", "test1")
       writeFile("Foo/foo2.txt", "test2")
       
       mkdir("Bar")
       
       getListOfFiles(new File("Foo")) should be 
        (List( new File("Foo/foo1.txt"), new File("Foo/foo2.txt")))
       
       getListOfFiles(new File("Bar")) should be (List[File]())
       
       removeDir("Foo")  
       removeDir("Bar")
  }
  
  "containsWord" should "correctly search in the file" in {

       mkdir("Foo")
       writeFile("Foo/foo1.txt", "  scalascala ")
       writeFile("Foo/foo2.txt", "scala scala")
              
       containsWord(new File("Foo/foo1.txt"), "scala").get should be (false)
   
       containsWord(new File("Foo/foo2.txt"), "scala").get should be (true)
       
       (containsWord(new File("Foo/foo3.txt"), "scala") match {
         case Success(b) => "Success"
         case Failure(ex) => "Failure"
       })  should be ("Failure")
       
       removeDir("Foo")  
       
  }
  
  "getResult function" should "handle invalid directory input" in {
      
       getResult(new File(""), "scala") should be (List("There is no such directory"))
       getResult(new File("Fab"),"scala") should be (List("There is no such directory"))
       getResult(new File("Fab///"), "scala") should be  (List("There is no such directory"))
              
  }
  
  "getResult function" should "correctly work for valid directory input" in {
      
       mkdir("Foo")
       writeFile("Foo/foo1.txt", "  scalascala ")
       writeFile("Foo/foo2.txt", "scala scala")
       
       mkdir("Foo/Bar")
       mkdir("Foo/Bar/Bar1")
       mkdir("Foo/Bar/Bar2")
       writeFile("Foo/Bar/Bar2/bar1.txt", "  scalascala ")
       writeFile("Foo/Bar/Bar2/bar2.txt", "scala scala")
       mkdir("Foo/Bar/Bar3")
       
       getResult(new File("Foo"), "scala") should be (List("Foo/foo2.txt", "Foo/Bar/Bar2/bar2.txt"))
       
       //removeDir("Foo") 
              
  }

}