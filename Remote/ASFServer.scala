import actors.Actor._
import actors.remote.{Node, RemoteActor}
import actors.remote.RemoteActor._
import scala.actors._
import java.lang.Math

/**
 * Created with IntelliJ IDEA.
 * User: KS
 * Date: 9/14/12
 * Time: 1:30 AM
 * To change this template use File | Settings | File Templates.
 */

case class ArithmaticSequence(first_num: Double, sequence_length: Int)
case class Sum(first_num: Double, sequence_length: Int)
case class Compare(first_num: Double, sum: Double)
case class CompareResult(first_num: Double)
case object Size
case class DoneByBoss(count: Double)

object Myexception extends Exception{
}

object ASFServer {
  var first_num: Double = 0
  var sequence_length = 0
  def main(args: Array[String]) {
    collect_values
    start_master_and_send_val
  }

  def collect_values{
    println("Enter the maximum possible First Number : ");
    first_num = readDouble()
    println("Number u entered : "+first_num)
    println("Enter the sequence length : ");
    sequence_length = readInt()
  }

  def start_master_and_send_val{
    //val boss_actor = new BossActor
    BossActor.start()
    BossActor ! ArithmaticSequence(first_num, sequence_length)
  }
}

object BossActor extends Actor{
  var slave_actors: Array[AbstractActor] = Array()
  var load_per_actor: Array[Double] = Array()
  var compare_actors: Array[AbstractActor] = Array()
  var max_iterations: Double = -1
  var max_actors_needed = 1

  def act(){
    RemoteActor.classLoader = getClass.getClassLoader
    alive(5000)
    register('bact, self)
    loop {
      react{
        case ArithmaticSequence(first_num, sequence_length) => process(first_num, sequence_length)
      }
    }
  }

  def process(first_num: Double, sequence_length: Int){
    println("In process");
    max_iterations = first_num
    create_actors
   // start_actors
    DataDisplayActor.start()
    DataDisplayActor.max_iterations = first_num
    //for(i <- 1 to first_num)
    try{
      var i: Double = 1
      var now_sent: Double = 0
      var tired_recurs = 1
      while(true){
        var index = (i % max_actors_needed).toInt
        slave_actors(0) ! Sum(i,sequence_length) //*****************************/
        println("Sent to slave actor")
        load_per_actor(index) += 1
        now_sent += 1
        if(now_sent >= 1e7){
          println("___Tired___")
          Thread.sleep(25000)
          now_sent = 0
          tired_recurs +=1
          println("___Woke Up__")
        }
        //if (index == 1) slave_actors(index) ! Size
        if(i == first_num){
          println("Actors size" + slave_actors.size)
          for (index <- load_per_actor){
            println("Load Per Actors "+ index)
          }
          for (index <- 0 to slave_actors.length - 1)
            slave_actors(index) ! DoneByBoss(load_per_actor(index))
          throw Myexception
        }
        i=i+1
      }
    }
    catch{
      case Myexception => ()
    }
  }

  def create_actors{
    //for (act <- 1 to 4){
      slave_actors:+= select(Node("127.0.0.1", 6000), 'sact)
      load_per_actor:+= 0.0
    //}
  }

/*  def start_actors{
    if (max_iterations <= slave_actors.length)
      max_actors_needed = (max_iterations).toInt
    else
      max_actors_needed = slave_actors.length
    for (index <- 0 to max_actors_needed - 1){
      slave_actors(index).start()
      compare_actors(index).start()
    }
  }            */
}

object DataDisplayActor extends Actor{
  var arr: Array[Double] = Array()
  var max_iterations: Double = -1
  var unequals:Int = 0
  var equals:Int = 0
  var calls_made: Double = 0
  def act(){
    RemoteActor.classLoader = getClass.getClassLoader
    alive(9000)
    register('dact, self)
    loop {
      react{
        case CompareResult(first_num: Double) => store_result(first_num)
        case DoneByComparator(load: Double) => calls_made += load; if (calls_made == max_iterations){
          println("Final Output")
          for (i <- arr)
            println(i)
          System.exit(0);
        }
      }
    }
  }

  def store_result(first_num: Double){
    //println("Calls Made : "+calls_made)
    //println("Max Iterations : "+max_iterations)
    if(first_num != -1)
    {
      println("Number : "+first_num)
      arr:+= first_num;
    }
    if (calls_made == max_iterations){
      println("Final Output")
      for (i <- arr)
        println(i)
      System.exit(0)
    }
  }
}
