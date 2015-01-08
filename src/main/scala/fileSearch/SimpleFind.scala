package fileSearch

import java.io.File
import scala.io.Source
import scala.util.Try

object SimpleFind{
  
  def main(args: Array[String]) {
   
     if (args.length !=2 ) {
       println("Usage: SimpleFind dirPath wordToBeSearched")
     } else {
        getResult(new File(args(0)), args(1)) map println     
    }
         
  }
  
  // getResult(f: File, word: String): List[String]
  // Helper functions are called and output string is computed
  //  Input: Word to be searched, and the file that is going to be searched
  //  Output:  List of strings  to be printed as output.
  
  def getResult(f: File, word: String): List[String] = {
    if (f.isDirectory() && f.exists()) {
        val resfiles = getListOfSubDirectories(f).flatMap(d => getListOfFiles(d).filter{ fi =>
             containsWord(fi, word).getOrElse(false)
            })
        resfiles.map(_.toString())
      }
      else (List("There is no such directory")) 
  }
	
  // getListOfSubDirectories(dir: String):List[File]
  // Recursively finds sub-directories in a given directory
	// Input: A file Object of directory type
	// Output:  Optional list of sub-directories. In case of an invalid directory path 
  // it returns None
	
	def getListOfSubDirectories(dir: File): List[File] = {
			def getallSubDirs(dir: File, accumdirList : List[File]): List[File] = {
			    val ls = dir.listFiles.filter(_.isDirectory).toList
			    ls  match {
			      case Nil => accumdirList
			      case _ => accumdirList++ ls  ++ ls.flatMap(d => getallSubDirs(d, List[File]()))
			    }
			}
			
  
		  getallSubDirs(dir, List(dir))
    
       
	}   
	

	// getListOfFiles(dir: File):List[File]
  // given a valid directory, returns all files in that directory
  // Input: a file Object of directory type
  // Output: list of files in that directory
  // given a valid directory, returns all files in that directory
  def getListOfFiles(dir: File):List[File] = {
      dir.listFiles.filter(_.isFile).toList
      
  }
  
  // containsWord(file: File, word: String):Try[Boolean]
  // Input: Word to be searched, and the file that is going to be searched
  // Output: Try[Boolean]
  // The method reads a file and searches it for the given word. The whole logic is wrapped in Try to 
  // deal with the case when there is an error in opening and reading the file.
  // The function assumes that a line can fit into default buffSize of Source.fromFile method.
  // Note that there can be the cases when a text file is not properly formed, e.g.,
  // a line is not properly terminated by  CR/LF.   
  def containsWord(file: File, word: String):Try[Boolean] = {
    Try{
      val bufferedSource = Source.fromFile(file)
      val ret = bufferedSource.getLines.foldLeft(false){(b,x) => 
        b || x.split(" ").contains(word)}
      bufferedSource.close
      ret   
    }
  }
  
}