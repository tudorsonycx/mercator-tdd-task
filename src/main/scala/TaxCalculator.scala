class TaxCalculator {

  // Tax bands (simplified to make testing a bit easier)
  private val personalAllowance: Int = 12570
  private val basicRateLimit: Int = 50270
  private val higherRateLimit: Int = 125140

  // Tax rates
  private val basicRate: Double = 0.2
  private val higherRate: Double = 0.4
  private val additionalRate: Double = 0.45


  // A method to calculate the total amount of tax to be paid, returned as a double
  def calculateTax(income: Double): Double = {
    val tax: Double = {
      val adjustedAllowance: Double =
        Math.max(0, Math.min((income - 100000) / 2, personalAllowance))

      val basicTax: Double =
        Math.max(Math.min(income, basicRateLimit) - personalAllowance, 0) * basicRate

      val higherTax: Double =
        (Math.max(Math.min(income, higherRateLimit) - basicRateLimit, 0) + adjustedAllowance) * higherRate

      val additionalTax: Double =
        Math.max(income - higherRateLimit, 0) * additionalRate

      basicTax + higherTax + additionalTax
    }
    f"$tax%.2f".toDouble
  }

  // A method which can tell you if someone is a higher rate taxpayer
  def isHigherRateTaxpayer(income: Double): Boolean = {
    income > 50270
  }

  // A method that will return a string with the income limit of their current tax band.
  // The return will also be formatted, E.g: "£12,500" or "No limit"
  def formattedCurrentTaxAllowance(income: Double): String = {
    ???
  }

}
