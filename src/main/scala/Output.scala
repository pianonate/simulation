/**
  * Created by rhialtotm on 3/15/17.
  * facade for print and println to facilitate testing
  */
trait Output {
  def print(s: String):Unit = Console.print(s)
  def println(s:String):Unit  = Console.println(s)
  def println():Unit = Console.println
}
