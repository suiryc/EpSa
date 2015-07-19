package epsa

import java.util.Date

/** Asset value: holds asset value at a given date. */
case class AssetValue(date: Date, value: Double)

/** Support: list of asset values. */
case class Support(name: String, values: List[AssetValue] = Nil)
