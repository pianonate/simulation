/**
 * Created by nathan on 1/10/17.
 */
import org.scalatest.{ FlatSpec, _ }

class SillySpec extends FlatSpec {

  "The string \"this is silly\"" should "be be made uppercase by toUpperCase" in {
    assert("THIS IS SILLY"==="this is silly".toUpperCase)
  }
}
