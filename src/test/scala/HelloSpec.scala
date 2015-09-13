import org.scalatest._

class HelloSpec extends FunSpec with Matchers {
  
  it("should pass the simplest of tests") {
	  "hello" should be ("hello")
  }
}
