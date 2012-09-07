import scala.actors.Actor
import scala.actors._
import java.lang.Math
import scala.testing.Benchmark
/**
 * Created with IntelliJ IDEA.
 * User: KS
 * Date: 9/6/12
 * Time: 2:12 AM
 * To change this template use File | Settings | File Templates.
 */

case class ArithmaticSequence(first_num: Int, sequence_length: Int)
case class Sum(first_num: Int, sequence_length: Int)
case class Compare(first_num: Int, sum: BigInt)
case class CompareResult(first_num: Int)

object ArithmaticSequenceFinder {

  var first_num = 0
  var sequence_length = 0
  def main(args: Array[String]) {
    collect_values
    println("Sys "+ System.nanoTime())
    start_master_and_send_val
  }

  def collect_values{
    println("Enter the maximum possible First Number : ");
    first_num = readInt()
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
  var arr: Array[Int] = Array()
  var max_iterations = -1
  var calls_made = 0
  def act(){
    loop {
      react{
        case ArithmaticSequence(first_num, sequence_length) => process(first_num, sequence_length)
        case CompareResult(first_num: Int) => store_result(first_num)
      }
    }
  }

  def process(first_num: Int, sequence_length: Int){
    max_iterations = first_num
    val sa1 = new SlaveActor
    sa1.start()
    for(i <- 1 to first_num)
      sa1 ! Sum(i,sequence_length)
  }

  def store_result(first_num: Int){
    calls_made += 1;
    if(first_num != -1)
      arr:+= first_num;
    if (calls_made == max_iterations){
      println("Final Output")
      for (i <- arr)
        println(i)
      println("Sys "+ System.nanoTime())
      System.exit(0)
    }
  }
}

class SlaveActor extends Actor {
  def act() {
    loop {
      react{
        case Sum(first_num,sequence_length) => {square_and_add(first_num,sequence_length)}
        case Compare(first_num: Int, sum: BigInt) => {compare(first_num,sum, sender)}
      }
    }
  }
  def square_and_add(first_num: Int,sequence_length: Int){
    var sum:BigInt = 0
    for (i <- first_num to first_num+sequence_length-1){
      sum += i*i
    }
    val sa2 = new SlaveActor
    sa2.start()
    sa2 ! Compare(first_num, sum)
  }
  def compare(first_num: Int,sum: BigInt, from: OutputChannel[Any]){
    val square_root: Double = Math.sqrt(sum.doubleValue())
    if(Math.floor(square_root) == square_root)
      BossActor ! CompareResult(first_num)
    else
      BossActor ! CompareResult(-1)
  }
}
