package epsa.model

/** Investment fund. */
case class InvestmentFund(name: String, values: List[Savings.AssetValue] = Nil)
