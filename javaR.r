
import org.rosuda.REngine.REXPMismatchException;
import org.rosuda.REngine.Rserve.RConnection;
import org.rosuda.REngine.Rserve.RserveException;

//add REngine.jar, Rserve-0.6.1.jar.zip, RserveEngine.jar libraries

 public class demo 
 {
     public static void main(String a[]) throws REXPMismatchException 
     {
         RConnection connection = null;
         try 
         {
             connection = new RConnection();
             connection.eval("source('D:/MyScript1.R')");
             connection.eval("myAdd1('D:/Data Analytics/Kaggle/act_test.csv')");
             System.out.println("Printed Successfully !!");
         }
         catch (RserveException e) 
         {
             e.printStackTrace();
         }
     }
 }
 
 
 
// R Script for the File MyScript1.R located at the location D:MyScript.R
// myAdd1=function(filename){
//
//	  abc<-read.csv(filename)
//	  write.csv(abc,file = "D:/output.csv")
//	}
// 
