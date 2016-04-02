package epsa.tools

import epsa.model.Savings
import java.nio.file.Path

trait InvestmentFundProber {

  def probe(path: Path): Option[Savings.AssetValueHistory]

}
