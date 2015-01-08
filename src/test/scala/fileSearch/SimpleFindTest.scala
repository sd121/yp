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
       
       getListOfSubDirectories(new File("Foo")).get should be 
        (List( new File("Foo"), new File("Foo/Bar"), new File("Foo/Bar/Bar1"), 
            new File("Foo/Bar/Bar2")))
       
       getListOfSubDirectories(new File("Foo/Bar/Bar2")).get should be (List(new File("Foo/Bar/Bar2")))
       
       getListOfSubDirectories(new File("")) should be (None)
       
       getListOfSubDirectories(new File("Fab")) should be (None)
       
       getListOfSubDirectories(new File("Fab///")) should be  (None)
       
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

}