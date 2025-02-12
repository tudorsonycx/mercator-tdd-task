import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec.AnyWordSpec

class TaxCalculatorSpec extends AnyWordSpec {

  val taxCalculator: TaxCalculator = new TaxCalculator

  // I've done the first test for you!
  "TaxCalculator.calculateTax" should {
    "return the total amount of tax to pay" when {
      "the income is below the personal tax limit" in {
        val result: Double = taxCalculator.calculateTax(5000)

        val expectedResult: Double = 0

        result shouldBe expectedResult
      }

      "the income is below the basic rate limit" in {
        val result: Double = taxCalculator.calculateTax(50270)

        val expectedResult: Double = 7540

        result shouldBe expectedResult
      }

      "the income is below the higher rate limit and below 100000" in {
        val result: Double = taxCalculator.calculateTax(100000)

        val expectedResult: Double = 27432

        result shouldBe expectedResult
      }

      "the income is above the higher rate limit" in {
        val result: Double = taxCalculator.calculateTax(345617.54)

        val expectedResult: Double = 141730.89

        result shouldBe expectedResult
      }
    }

    "return 0" when {
      "the income is a negative number" in {
        val result: Double = taxCalculator.calculateTax(-12339)

        val expectedResult: Double = 0

        result shouldBe expectedResult
      }
    }
  }

  "TaxCalculator.isHigherRateTaxpayer" should {
    "return false" when {
      "the income is below the higher rate limit" in {
        val result: Boolean = taxCalculator.isHigherRateTaxpayer(50270)

        val expectedResult: Boolean = false

        result shouldBe expectedResult
      }

      "the income is a negative number" in {
        val result: Boolean = taxCalculator.isHigherRateTaxpayer(-13012)

        val expectedResult: Boolean = false

        result shouldBe expectedResult
      }
    }

    "return true" when {
      "the income is withing the higher rate limit" in {
        val result: Boolean = taxCalculator.isHigherRateTaxpayer(125140)

        val expectedResult: Boolean = true

        result shouldBe expectedResult
      }

      "the income is above the higher rate limit" in {
        val result: Boolean = taxCalculator.isHigherRateTaxpayer(125141)

        val expectedResult: Boolean = true

        result shouldBe expectedResult
      }
    }
  }
}
