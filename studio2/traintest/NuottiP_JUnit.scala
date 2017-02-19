package studio2.traintest

import scala.collection.mutable.Buffer
import scala.collection.mutable.Map
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.Assertions._
import studio2.train._


@RunWith(classOf[JUnitRunner])
class NuottiPTest extends FlatSpec with Matchers {
    
  
  // Fill in the tests below. The first one is given as an example. 
  
   "NuottiPiirturi" should "find non-valid note names (not investigating length)" in {
     val nuottejaV = Buffer("t1-", "d3-", "f",  "p", "1c", "2f", "f2f2", "f2f", "f2f----", "hb22----", "c11", "d#b", "hb#", 
         "ddd", "d1d", "ab2", "a#2----", "hb2", "a2", "#c" )  // ,  "h2"
     val taukojaV= Buffer("za", "z#--", "zz", "zz-top", "tauko")     
    
     val nuottejaO = Buffer("d1#.----", "e1----.#", "c1-.", "g1-", "H#2", "Gb1----", "Ab1--",  "f#1---", "d2b", "a-----1#",
         "--c1", "----g2")  // 
     val taukojaO= Buffer("z", "z--", "z----", "z.", "-z")   
     
     var virheita = 0 
    
     for (nuotti <- nuottejaO) {
       
        val filtteredNote = nuotti.filter(_ != '-').filter(_ != '.')
   //     println(filtteredNote)
         
        if(filtteredNote == "z")
          {}
        else{
          if(!"cdefgah".contains(filtteredNote.toLowerCase().head.toString()))
              virheita += 1
          else if(!(filtteredNote.tail.contains("1")|| filtteredNote.tail.contains("2")))   
            virheita += 1  
          else if(filtteredNote.size < 2)
            virheita += 1
          else if(filtteredNote.size > 3)
            virheita += 1  
          else if(filtteredNote.tail.contains("#b") ||   filtteredNote.tail.contains("#b"))    
              virheita += 1 
          else if  (filtteredNote.contains("a") &&  filtteredNote.contains("2"))  // löytää a2, a#2, ab2 = piirtoalueen ulkopuolella    
              virheita += 1      
          else if(filtteredNote.contains("h") &&   filtteredNote.contains("2"))  // löytää h2, h#2, hb2 = piirtoalueen ulkopuolella    
              virheita += 1               
          else if(filtteredNote.size == 3){   // tämä testi vikana !!!
              if(!(filtteredNote.tail.contains("#") || filtteredNote.tail.contains("b")))   
                 virheita += 1  
          }    
        } // iso else
    }  // for
      assert(virheita == nuottejaO.size)   
   } 
  

  
  /* BU , ilman taukoja: 
   
   "NuottiPiirturi" should "find non-valid note names (not investigating length)" in {
     val nuottejaV = Buffer("t1-", "d3-", "f",  "p", "1c", "2f", "f2f2", "f2f", "f2f----", "hb22----", "c11", "d#b", "hb#", 
         "ddd", "d1d", "ab2", "a#2----", "hb2", "a2" )  // ,  "h2"
    
     val nuottejaO = Buffer("d1#.----", "e1----.#", "c1-.", "g1-", "H#2", "Gb1----", "Ab1--",  "f#1---", "d2b", "a-----1#")  // 
    
    var virheita = 0 
    for (nuotti <- nuottejaO) {
       
        val filtteredNote = nuotti.filter(_ != '-').filter(_ != '.')
   //     println(filtteredNote)
         
        if(!"cdefgah".contains(filtteredNote.toLowerCase().head.toString()))
            virheita += 1
        else if(!(filtteredNote.tail.contains("1")|| filtteredNote.tail.contains("2")))   
          virheita += 1  
        else if(filtteredNote.size < 2)
          virheita += 1
        else if(filtteredNote.size > 3)
          virheita += 1  
        else if(filtteredNote.tail.contains("#b") ||   filtteredNote.tail.contains("#b"))    
            virheita += 1 
        else if  (filtteredNote.contains("a") &&  filtteredNote.contains("2"))  // löytää a2, a#2, ab2 = piirtoalueen ulkopuolella    
                    virheita += 1  
            
        else if(filtteredNote.contains("h") &&   filtteredNote.contains("2"))  // löytää h2, h#2, hb2 = piirtoalueen ulkopuolella    
            virheita += 1       
             
        else if(filtteredNote.size == 3){   // tämä testi vikana !!!
            if(!(filtteredNote.tail.contains("#") || filtteredNote.tail.contains("b")))   
             virheita += 1  
        }     
    }  
      assert(virheita == nuottejaO.size)   
   } 
  
 */
   
    "NuottiPiirturi" should "find non-valid length)" in {
     val nuottejaV = Buffer("d1#.----", "f2-----", "g1.", "h1---------", "a1---."  )
     val nuottejaO = Buffer( "c1", "c1-", "c1-.", "e1--#",  "H#2--.", "Gb1---", "Ab1----")  // kaikki sallitut pituudet
    
    var virheita = 0 
    for (nuotti <- nuottejaO) {
        val lkm = nuotti.count(_ == '-')
        println(lkm)
        if(lkm > 4)
            virheita += 1
        else if(lkm == 3 && nuotti.contains("."))    // ohjelmassa ei määritelty pisteellistä pisteellistä puolinuottia
          virheita += 1   
        else if(lkm == 4 && nuotti.contains("."))   // max pituus 4
          virheita += 1  
        else if(lkm == 0 && nuotti.contains("."))    // ei pisteellistä kahdeksasosaa
          virheita += 1     
      }     
      assert(virheita == nuottejaO.size)   
   } 

  
}