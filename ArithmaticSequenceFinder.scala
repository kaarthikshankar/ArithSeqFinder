import management.ManagementFactory
import scala.actors.Actor
import scala.actors._
import java.lang.Math
import com.sun.management.OperatingSystemMXBean

import scala.testing.Benchmark
/**
 * Created with IntelliJ IDEA.
 * User: KS
 * Date: 9/6/12
 * Time: 2:12 AM
 * To change this template use File | Settings | File Templates.
 */

case class ArithmaticSequence(first_num: Double, sequence_length: Int)
case class Sum(first_num: Double, sequence_length: Int, comparator: Actor)
case class Compare(first_num: Double, sum: Double)
case class CompareResult(first_num: Double)
case object Size
case class DoneByBoss(count: Double, comparator: Actor)
case class DoneByPeer(count: Double)
case class DoneByComparator(load: Double)

object Myexception extends Exception{
}

object Systime{
  var time:Long = 0
  var cpu_time:Long = 0
}

object ArithmaticSequenceFinder {

  var first_num: Double = 0
  var sequence_length = 0
  def main(args: Array[String]) {
    collect_values
    println("Sys "+ System.nanoTime())
    Systime.time = System.nanoTime();
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
  var slave_actors: Array[SlaveActor] = Array()
  var load_per_actor: Array[Double] = Array()
  var compare_actors: Array[SlaveActor] = Array()
  var max_iterations: Double = -1
  var max_actors_needed = -1

  def act(){
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
    start_actors
    DataDisplayActor.start()
    DataDisplayActor.max_iterations = first_num
    //for(i <- 1 to first_num)
    try{
      var i: Double = 1
      var now_sent: Double = 0
      var tired_recurs = 1
      while(true){
        var index = (i % max_actors_needed).toInt
        slave_actors(index) ! Sum(i,sequence_length, compare_actors(index))
        load_per_actor(index) += 1
        now_sent += 1
        if(now_sent >= 1e7){
          println("___Tired___")
          Thread.sleep(40000)
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
            slave_actors(index) ! DoneByBoss(load_per_actor(index), compare_actors(index))
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
    for (act <- 1 to 4){
      slave_actors:+= new SlaveActor
      compare_actors:+= new SlaveActor
      load_per_actor:+= 0.0
    }
  }

  def start_actors{
    if (max_iterations <= slave_actors.length)
      max_actors_needed = (max_iterations).toInt
    else
      max_actors_needed = slave_actors.length
    for (index <- 0 to max_actors_needed - 1){
      slave_actors(index).start()
      compare_actors(index).start()
    }
  }
}

class SlaveActor extends Actor {
  var my_load: Double = 0
  def act() {
    loop {
      react{
        case Sum(first_num,sequence_length, comparator) => {square_and_add(first_num,sequence_length,comparator)}
        case Size => println("Mail Box Size " + mailboxSize);
        case Compare(first_num: Double, sum: Double) => {compare(first_num,sum, sender)}
        case DoneByBoss(load: Double, comparator: Actor) => {my_load = load; comparator ! DoneByPeer(my_load)}
        case DoneByPeer(load: Double) => DataDisplayActor ! DoneByComparator(load)
      }
    }
  }
  def square_and_add(first_num: Double,sequence_length: Int, comparator: Actor){
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
    comparator ! Compare(first_num, sum)
    if(my_load > 0) {println("Done by boss");comparator ! DoneByPeer(my_load)}
  }

  def compare(first_num: Double,sum: Double, from: OutputChannel[Any]){

    val square_root: Double = Math.sqrt(sum)
    if(Math.floor(square_root) == square_root)
    {
      DataDisplayActor ! CompareResult(first_num)
    }
  }
}

object DataDisplayActor extends Actor{
  var arr: Array[Double] = Array()
  var max_iterations: Double = -1
  var unequals:Int = 0
  var equals:Int = 0
  var calls_made: Double = 0
  def act(){
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
      println("Sys "+ System.nanoTime())
      Systime.time = System.nanoTime() - Systime.time
      println("Difference in nanoseconds : " + Systime.time)
      var bean: OperatingSystemMXBean = (ManagementFactory.getOperatingSystemMXBean).asInstanceOf[(com.sun.management.OperatingSystemMXBean)]
      println("CPU TIME : " + bean.getProcessCpuTime)
      System.exit(0)
    }
  }
}