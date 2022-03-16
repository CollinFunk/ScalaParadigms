package oop.lab5

import java.util.concurrent.atomic.AtomicInteger


// ++++++++++++++++++++++++
// Implementing a reference class
// ++++++++++++++++++++++++

// given
enum LetterGrade:
  case A, B, C, D, F

class Assignment(val name: String, val assignment: Int, var grade: Int):
  var letterGrade: LetterGrade = LetterGrade.F
  if (grade < 0 || grade > 100) throw Exception("Invalid grade, must be between 0 and 100")
  if (grade >= 90) letterGrade = LetterGrade.A
  else if (grade >= 80) letterGrade = LetterGrade.B
  else if (grade >= 70) letterGrade = LetterGrade.C
  else if (grade >= 60) letterGrade = LetterGrade.D
  else if (grade >= 0) letterGrade = LetterGrade.F

  override def toString: String = name + " assn " + assignment + ": " + grade + " (= " + letterGrade + ")"


object testAssignment extends App:
  try
    val simpson = Assignment("Simpson", 1, -88)
  catch
    case e: Exception => println(e.getMessage) // Invalid grade

  val jones = Assignment("Jones", 1, 88)
  val hanson = Assignment("Hanson", 1, 95)

  println(jones.grade) // 88
  println(jones.letterGrade) // B
  jones.grade = jones.grade + 10
  println(jones.grade) //98
  println(jones.letterGrade) // A
  println(jones) // Jones assn 1: 98 (= A)

  println(hanson.grade) // 95
  println(hanson.letterGrade) // A
  try
    hanson.grade = hanson.grade + 10
  catch
    case e: Exception => println(e.getMessage) // Invalid grade
  finally
    println(hanson) // Hanson assn 1: 95 (= A)
  try
    val smith = Assignment("Smith", 1, -10)
  catch
    case e: Exception => println(e.getMessage) // Invalid grade


// ++++++++++++++++++++++++
// Implementing static variables & methods
// ++++++++++++++++++++++++

final val idGenerator = new AtomicInteger(500)
class Transaction(val fromAcc: Int, val toAcc: Int, val amt: Double):
  val transactionID: Int = idGenerator.getAndIncrement()
  if (fromAcc <= 0 || toAcc <= 0 || amt <= 0) throw Exception("Invalid amount")

  override def toString: String = "Transaction #" + transactionID.toString + ": $" + amt + " from acct " + fromAcc + " to acct " + toAcc



object testTransactions extends App:
  try
    val t1 = Transaction(119, 212, -20.50)
  catch
    case e: Exception => println(e.getMessage) // Invalid amount
  val ledger = List(
    Transaction(119, 212, 600.50),
    Transaction(212, 119, 1200),
    Transaction(212, 119, 98.75)
  )
  ledger.foreach(println) // how to create unique IDs
/*
Transaction #500: $600.5 from acct 119 to acct 212
Transaction #501: $1200.0 from acct 212 to acct 119
Transaction #502: $98.75 from acct 212 to acct 119
*/

// ++++++++++++++++++++++++
// Implementing a value class
// ++++++++++++++++++++++++


class Time (val hours: Int, val minutes: Int) extends Ordered[Time]:
  if (hours < 0 || hours > 23) throw Exception("Invalid Hour")
  if (minutes < 0 || minutes > 59) throw Exception("Invalid minute")

  def this(hours: Int) =
    this(hours, 0)

  def this(time: String) =
    this(Integer.parseInt(time.split(":")(0)),Integer.parseInt(time.split(":")(1)) )

  override def toString: String = hours + ":" + String.format("%02d", minutes)
  override def hashCode(): Int = this.toString.hashCode
  override def equals(other: Any): Boolean =
    other match {
      case p: Time => p.isInstanceOf[Time] && (p.hours == this.hours) && (p.minutes == this.minutes)
      case _ => false
    }

  override def compare(that: Time): Int =
    if (this.hours < that.hours)
      -1
    else if (this.hours == that.hours)
      if (this.hours < that.hours)
        1
      else if (this.hours > that.hours)
        -1
      else
        0
    else
      1

  def +(that: Time) =
    if (this.hours + that.hours % 23 != 0 && this.hours + that.hours > 23)
      if(this.minutes + that.minutes > 59)
        new Time(((this.hours + that.hours) % 24) + 1, (this.minutes + that.minutes) % 60)
      else
        new Time(((this.hours + that.hours) % 24), this.minutes + that.minutes)
    else
      if(this.minutes + that.minutes > 59)
        new Time(this.hours + that.hours, (this.minutes + that.minutes) % 60)
      else
        new Time(this.hours + that.hours, this.minutes + that.minutes)


class PreciseTime(hours: Int, minutes: Int, val seconds: Int) extends Time(hours, minutes):
  if (seconds >= 60) throw Exception("Invalid seconds")
  def this (hours: Int, minutes: Int) =
    this(hours, minutes, 0)

  def this (hours: Int) =
    this(hours, 0, 0)

  override def toString: String = super.toString + ":" +  String.format("%02d", seconds)
  override def equals(other: Any): Boolean =
    other match {
      case p: PreciseTime => p.isInstanceOf[PreciseTime] && (p.hours == this.hours) && (p.minutes == this.minutes) && (p.seconds == this.seconds)
      case _ => false
    }


object testTime extends App:
  try
    val t = Time(24, 50)
  catch
    case e: Exception => println(e.getMessage) // Invalid hour
  try
    val t = Time(12, 60)
  catch
    case e: Exception => println(e.getMessage) // Invalid minute

  val t1 = Time(10, 30)
  val t2 = Time(15, 45)
  val t3 = Time(10, 30)
  val t4 = Time("18:45")
  val t5 = Time(17)
  val t6 = t1 + t2
  println(t1) // 10:30
  println(t2) // 15:45
  println(t3) // 10:30
  println(t4) // 18:45
  println(t5) // 17:00
  println(t6) // 2:15
  println(t1 == t3) // true
  println(t1 != t5) // true
  println(t1 < t2)  // true
  println(t4 < t2)  // false
  println(t1 <= t3) // true

  val schedule = Map(
    t1 -> "coffee break",
    t2 -> "nap",
    t4 -> "cocktail hour"
  )
  println(schedule) // Map(10:30 -> coffee break, 15:30 -> nap, 18:45 -> cocktail hour)
  println(schedule(t3)) // coffee break

  try
    val pt = PreciseTime(12, 0, 60)
  catch
    case e: Exception => println(e.getMessage) // Invalid second

  val pt2 = PreciseTime(18)
  val pt1 = PreciseTime(10, 30)
  println(pt1) // 10:30:00
  println(pt2) // 18:00:00
  println(t1) // 10:30
  println(pt1 == t1) // false
  try
    println(schedule(pt1))
  catch
    case e: Exception => println(e.getMessage) // key not found: 10:30:00
