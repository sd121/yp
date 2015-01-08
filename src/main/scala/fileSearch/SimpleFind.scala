package fileSearch

import java.io.File
import scala.io.Source
import scala.util.Try

object SimpleFind{
  

	
  // getListOfSubDirectories(dir: String):List[File]
  // Recursively finds sub-directories in a given directory
	// Input: A file Object of directory type
	// Output:  Optional list of sub-directories. In case of an invalid directory path 
  // it returns None
	
	def getListOfSubDirectories(dir: File): Option[List[File] ]= {
			def getallSubDirs(dir: File, accumdirList : List[File]): List[File] = {
			    val ls = dir.listFiles.filter(_.isDirectory).toList
			    ls  match {
			      case Nil => accumdirList
			      case _ => accumdirList++ ls  ++ ls.flatMap(d => getallSubDirs(d, List[File]()))
			    }
			}
			
      if (dir.isDirectory())
		     Some(getallSubDirs(dir, List(dir)))
      else None
       
	}   
	

	// getListOfFiles(dir: File):List[File]
  // given a valid directory, returns all files in that directory
  // Input: a file Object of directory type
  // Output: list of files in that directory
  // given a valid directory, returns all files in that directory
  def getListOfFiles(dir: File):List[File] = {
      dir.listFiles.filter(_.isFile).toList
      
  }
  
}