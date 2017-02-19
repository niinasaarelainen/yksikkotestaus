package studio2.traintest

import scala.collection.mutable.Map
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.Assertions._
import studio2.train._

/**
 * Tests for BidBot
 */
@RunWith(classOf[JUnitRunner])
class TrainTest extends FlatSpec with Matchers {
  
  class TestCar(val number: Int) extends TrainCar {
    def numberOfPlaces     = ???
    def numberOfFreePlaces = ???
    def reservePlaces(numberOfPeople: Int) = ???    
  }
  
  "Train" should "allow adding the maximum number of cars" in {
    val train = new Train("HKI-OULU")

    for (i <- 1 to Train.MaximumLength) {
      val s = new TestCar(i)
      val success = train.addCar(s)
      assert(success, "Train failed to add a car")
    }
  }

  "Train.car" should "return the correct car" in {
    val train = new Train("HKI-OULU")
    assert(train.car(1) == None, "There should not be a car in an empty train")
    assert(train.car(2) == None, "There should not be a car in an empty train")
    
    val firstCar = new TestCar(1)
    train.addCar(firstCar)

    assert(train.car(1).contains(firstCar), "The first car should be the one just added")

    val secondCar = new TestCar(2)

    train.addCar(secondCar)

    assert(train.car(1).contains(firstCar),  "The first  car should be the one just added")
    assert(train.car(2).contains(secondCar), "The second car should be the one just added")
    assert(train.car(3) == None, "The third car should not be there")    
  }
  
  "Traincar" should "calculate the fullness properly" in {
    val testCar = new TrainCar {
      val numberOfPlaces = 200
      val numberOfFreePlaces = 199
      def reservePlaces(numberOfPeople: Int) = ???
    }
    
    assert(testCar.fullness == 0.5, "1 seats out of 20 should be 5.0%")
  }
  
  "SittingCar.findAdjacents" should "return the correct number adjacent seats" in {
    val sittingCar = new SittingCar(1,3)
    sittingCar.reservePlace(1, 'a')
    assert(sittingCar.reservePlaces(3) == false, "There should not be room")
  }

  "SittingCar.reserveAdjacents" should "reserve all the adjacent seats" in {
    val sittingCar = new SittingCar(1,3)
    sittingCar.reservePlaces(3)
    assert(sittingCar.isReservedSeat(1, 'b'), "Seat 1b should be reserved")
  }

  "SleepingCar.numberOfFreePlaces" should "only count beds in fully empty cabins" in {
    val sleepingCar = new SleepingCar(2)
    sleepingCar.reserveCabin(1)
    assert(sleepingCar.numberOfFreePlaces == SleepingCar.BedsPerCabin, "There should be one cabinfull of beds left")
  }
  
  "SleepingCar.reserveCabin" should "tell if reservation was a success" in {
    val sleepingCar = new SleepingCar(1)
    assert(sleepingCar.reserveCabin(1), "reserving this one cabin should have succeeded")    
  }
  
  "SleepingCar.resercePlaces" should "actually reserve the cabins" in {
    val sleepingCar = new SleepingCar(2)
    sleepingCar.reservePlaces(1)
    assert(sleepingCar.isEmptyCabin(1) == false, "reserving this one cabin should have succeeded")    
    
  }
}
