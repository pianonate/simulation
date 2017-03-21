/**
  * Created by rhialtotm on 3/15/17.
  * trait to facilitate unit testing of print and println
  */
trait MockOutput extends Output {
  var messages: Seq[String] = Seq()
  override def print(s:String):Unit = messages = messages :+ s
  override def println(s:String):Unit = messages = messages :+ (s + "\n")
  override def println():Unit = messages = messages :+ "\n"
}
