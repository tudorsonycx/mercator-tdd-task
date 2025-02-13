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

  "TaxCalculator.formattedCurrentTaxAllowance" should {
    "return the personal allowance" when {
      "the income is below the personal allowance limit" in {
        val result: String = taxCalculator.formattedCurrentTaxAllowance(12570)

        val expectedResult: String = f"£${taxCalculator.personalAllowance}%,d"

        result shouldBe expectedResult
      }

      "the income is a negative number" in {
        val result: String = taxCalculator.formattedCurrentTaxAllowance(-498112)

        val expectedResult: String = f"£${taxCalculator.personalAllowance}%,d"

        result shouldBe expectedResult
      }
    }

    "return the basic rate limit" when {
      "the income is below the basic rate limit" in {
        val result: String = taxCalculator.formattedCurrentTaxAllowance(50270)

        val expectedResult: String = f"£${taxCalculator.basicRateLimit}%,d"

        result shouldBe expectedResult
      }
    }

    "return the higher rate limit" when {
      "the income is below the higher rate limit" in {
        val result: String = taxCalculator.formattedCurrentTaxAllowance(125140)

        val expectedResult: String = f"£${taxCalculator.higherRateLimit}%,d"

        result shouldBe expectedResult
      }
    }

    "return No limit" when {
      "the income is above the higher rate limit" in {
        val result: String = taxCalculator.formattedCurrentTaxAllowance(231512)

        val expectedResult: String = "No limit"

        result shouldBe expectedResult
      }
    }
  }

  "TaxCalculator.calculateCapitalGainTaxOnShares" should {
    "return the cgt on shares" when {
      "the total income is below basic rate limit" in {
        val result: Double = taxCalculator.calculateCapitalGainTaxOnShares(20000, 30000)

        val expectedResult: Double =  1700

        result shouldBe expectedResult
      }

      "the total income is above basic rate limit" in {
        val result: Double = taxCalculator.calculateCapitalGainTaxOnShares(30000, 30000)

        val expectedResult: Double = 5400

        result shouldBe expectedResult
      }
    }

    "return 0" when {
      "the shares capital gain is less than capital gains allowance" in {
        val result: Double = taxCalculator.calculateCapitalGainTaxOnShares(2000, 120000)

        val expectedResult: Double = 0

        result shouldBe expectedResult
      }

      "the shares capital gain is negative" in {
        val result: Double = taxCalculator.calculateCapitalGainTaxOnShares(-2312.12, 120000)

        val expectedResult: Double = 0

        result shouldBe expectedResult
      }
    }
  }
}
