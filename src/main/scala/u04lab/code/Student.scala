package u04lab.code

import List.*

trait Student:
  def name: String
  def year: Int
  def enrolling(course: Course*): Unit // the student participates to a Course
  def courses: List[String] // names of course the student participates to
  def hasTeacher(teacher: String): Boolean // is the student participating to a course of this teacher?

trait Course:
  def name: String
  def teacher: String

case class StudentImpl(name: String, year: Int = 2017) extends Student:
  private var coursesSet: List[Course] = Nil()

  override def enrolling(newCourses: Course*) = newCourses.foreach(course =>
    coursesSet = List.append(Cons(course, Nil()), coursesSet))
  override def courses: List[String] = List.map(coursesSet)(course => course.name)
  override def hasTeacher(teacher: String): Boolean = List.contains(List.map(coursesSet)(course => course.teacher), teacher)

case class CourseImpl(name: String, teacher: String) extends Course

object Student:
  def apply(name: String, year: Int = 2017): Student = StudentImpl(name, year)

object Course:
  def apply(name: String, teacher: String): Course = CourseImpl(name, teacher)

@main def checkStudents(): Unit =
  val cPPS = Course("PPS", "Viroli")
  val cPCD = Course("PCD", "Ricci")
  val cSDR = Course("SDR", "D'Angelo")
  val s1 = Student("mario", 2015)
  val s2 = Student("gino", 2016)
  val s3 = Student("rino") // defaults to 2017
  s1.enrolling(cPPS, cPCD)
  s2.enrolling(cPPS)
  s3.enrolling(cPPS, cPCD, cSDR)
  println(
    (s1.courses, s2.courses, s3.courses)
  ) // (Cons(PCD,Cons(PPS,Nil())),Cons(PPS,Nil()),Cons(SDR,Cons(PCD,Cons(PPS,Nil()))))
  println(s1.hasTeacher("Ricci")) // true

/** Hints:
  *   - simply implement Course, e.g. with a case class
  *   - implement Student with a StudentImpl keeping a private Set of courses
  *   - try to implement in StudentImpl method courses with map
  *   - try to implement in StudentImpl method hasTeacher with map and find
  *   - check that the two println above work correctly
  *   - refactor the code so that method enrolling accepts a variable argument Course*
  */
