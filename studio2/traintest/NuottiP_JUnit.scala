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
    
  
  
"NuottiPiirturi" should "find non-valid note names (not investigating length)" in {
     val nuottejaVaarin = Buffer("t1-", "d3-", "f",  "p", "1c", "2f", "f2f2", "f2f", "f2f----", "hb22----", "c11", "d#b", "hb#", 
         "ddd", "d1d", "ab2", "a#2----", "hb2", "a2", "#c" )  // ,  "h2"
     val taukojaVaarin= Buffer("za", "z#--", "zz", "zz-top", "tauko", " z")     
    
     val nuottejaOikein = Buffer("d1#.----", "e1----.#", "c1-.", "g1-", "H#2", "Gb1----", "Ab1--",  "f#1---", "d2b", "a-----1#",
         "--c1", "----g2")  // 
     val taukojaOikein= Buffer("z", "z--", "z----", "z.", "-z", "z--.", "--.z")   
     
   
     var virheitaHylattavillaNuoteilla, virheitaHyvaksyttavillaNuoteilla, virheitaHylattavillaTauoilla, virheitaHyvaksyttavillaTauoilla = 0
    
     virheitaHylattavillaNuoteilla = laskeVirheet( nuottejaVaarin)
     virheitaHyvaksyttavillaNuoteilla =  laskeVirheet( nuottejaOikein)
     virheitaHylattavillaTauoilla = laskeVirheet( taukojaVaarin)
     virheitaHyvaksyttavillaTauoilla = laskeVirheet( taukojaOikein)
     
   def laskeVirheet(syotteet: Buffer[String]) = {   
      var virheita = 0 
      for (syote <- syotteet) {
       
         val filtteredNote = syote.filter(_ != '-').filter(_ != '.')
         
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
            else if(filtteredNote.tail.contains("#b") ||  filtteredNote.tail.contains("#b"))    
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
      
      virheita
}
     
      assert(virheitaHyvaksyttavillaNuoteilla == 0  &&  virheitaHylattavillaNuoteilla == nuottejaVaarin.size && 
          virheitaHyvaksyttavillaTauoilla == 0 &&  virheitaHylattavillaTauoilla == taukojaVaarin.size)  
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
   
"NuottiPiirturi" should "find non-valid lengths)" in {
     val pituuksiaVaarin = Buffer("d1#.----", "f2-----", "g1.", "h1---------", "a1---."  )
     val pituuksiaOikein = Buffer( "c1", "c1-", "c1-.", "e1--#",  "H#2--.", "Gb1---", "Ab1----")  // kaikki sallitut pituudet
    
     var virheitaHylattavillaPituuksilla, virheitaHyvaksyttavillaPituuksilla = 0
     
     virheitaHylattavillaPituuksilla = laskeVirheet( pituuksiaVaarin)
     virheitaHyvaksyttavillaPituuksilla =  laskeVirheet( pituuksiaOikein)
    
     
   def laskeVirheet(syotteet: Buffer[String] ) = {
     var virheita = 0 
     for (syote <- syotteet) {
        val lkm = syote.count(_ == '-')
        println(lkm)
        if(lkm > 4)
            virheita += 1
        else if(lkm == 3 && syote.contains("."))    // ohjelmassa ei määritelty pisteellistä pisteellistä puolinuottia
          virheita += 1   
        else if(lkm == 4 && syote.contains("."))   // max pituus 4
          virheita += 1  
        else if(lkm == 0 && syote.contains("."))    // ei pisteellistä kahdeksasosaa
          virheita += 1     
     }     
    
     virheita
   }  
    
      assert(virheitaHyvaksyttavillaPituuksilla == 0  &&  virheitaHylattavillaPituuksilla == pituuksiaVaarin.size )   
   } 

  
}