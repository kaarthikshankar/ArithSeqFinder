import actors.{OutputChannel, Actor}
import actors.Actor._
import actors.remote.RemoteActor
import actors.remote.RemoteActor._
import scala.actors.remote.Node
import actors.Actor._
import actors.remote.{Node, RemoteActor}
import actors.remote.RemoteActor._
import scala.actors._
import java.lang.Math

/**
 * Created with IntelliJ IDEA.
 * User: KS
 * Date: 9/14/12
 * Time: 1:39 AM
 * To change this template use File | Settings | File Templates.
 */

case class ArithmaticSequence(first_num: Double, sequence_length: Int)
case class Sum(first_num: Double, sequence_length: Int)
case class Compare(first_num: Double, sum: Double)
case class CompareResult(first_num: Double)
case object Size
case class DoneByBoss(count: Double)
case class DoneByPeer(count: Double)
case class DoneByComparator(load: Double)

object Myexception extends Exception{
}

object AFSClient {
  def main(args: Array[String]){
    SlaveActor.start()
    CompareActor.start()
  }
}

object SlaveActor extends Actor {
  var my_load: Double = 0
  def act() {
    RemoteActor.classLoader = getClass.getClassLoader
    alive(6000)
    register('sact, self)
    println("Registering slave actor")
    loop {
      react{
        case Sum(first_num,sequence_length) => {println("FRom server : "+first_num);square_and_add(first_num,sequence_length)}
        case Size => println("Mail Box Size " + mailboxSize);
        case DoneByBoss(load: Double) => {my_load = load; CompareActor ! DoneByPeer(my_load)}
      }
    }
  }
  def square_and_add(first_num: Double,sequence_length: Int){
    var sum:Double = 0
    try{
      var i:Double = first_num
      while(true){
        sum += i*i
        if (i==first_num+sequence_length-1)
          throw Myexception
        i=i+1

      }
    }
    catch{
      case Myexception => ()
    }
    CompareActor ! Compare(first_num, sum)
    if(my_load > 0) {println("Done by boss");CompareActor ! DoneByPeer(my_load)}
  }
}


object CompareActor extends Actor {
  var my_load: Double = 0
  var data_display = select(Node("127.0.0.1", 9000), 'dact)
  def act() {
    RemoteActor.classLoader = getClass.getClassLoader
    alive(8000)
    register('cact,self)
    loop {
      react{
        case Size => println("Mail Box Size " + mailboxSize);
        case Compare(first_num: Double, sum: Double) => {compare(first_num,sum, sender)}
        case DoneByPeer(load: Double) => {data_display ! DoneByComparator(load) ; Thread.sleep(100);System.exit(0)}
      }
    }
  }

  def compare(first_num: Double,sum: Double, from: OutputChannel[Any]){

    val square_root: Double = Math.sqrt(sum)
    if(Math.floor(square_root) == square_root)
    {
      data_display ! CompareResult(first_num)
    }
  }
}


