package epsa.model

import java.util.Date

/** Asset value: holds asset value at a given date. */
case class AssetValue(date: Date, value: Double)

/** Investment fund. */
case class InvestmentFund(name: String, values: List[AssetValue] = Nil)
