import org.scalatest.FlatSpec

/**
  * Created by nathanmccoy on 3/13/17.
  * simplify working with Context in tests
  */
abstract class ContextSpec extends FlatSpec {

  val gameSeedArg = "-" + Conf.gameSeedArg + "1"
  val sessionSeedArg = "-" + Conf.sessionSeedArg + "1"
  val fixedWeightArg = "-" + Conf.fixedWeightArg

  def getContext():Context = getContext(Array[String]())

  def getContext(arg:String):Context = getContext(Array(arg))

  def getContext(args: Array[String]):Context = {
    val context = new Context(new Conf(args)) with MockOutput
    context
  }

}
