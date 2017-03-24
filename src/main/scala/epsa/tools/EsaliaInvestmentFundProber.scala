package epsa.tools

import epsa.model.Savings
import java.io.ByteArrayInputStream
import java.nio.file.Path
import java.time.LocalDate
import java.time.format.DateTimeFormatter
import org.apache.poi.ss.usermodel.{Workbook, WorkbookFactory}
import suiryc.scala.io.PathsEx

/**
 * Esalia investment fund file prober.
 *
 * Probes whether a given file contains investment fund information (name and
 * asset values) provided by Esalia site (excel file).
 */
object EsaliaInvestmentFundProber extends InvestmentFundProber {

  override def probe(path: Path): Option[Savings.AssetValueHistory] = {
    val extension = PathsEx.extension(path).toLowerCase
    try {
      if ((extension == "xls") || (extension == "xlsx")) {
        probeExcel(WorkbookFactory.create(path.toFile))
      } else None
    } catch {
      case ex: Exception =>
        ex.printStackTrace()
        None
    }
  }

  def probe(raw: Array[Byte]): Option[Savings.AssetValueHistory] = {
    probeExcel(WorkbookFactory.create(new ByteArrayInputStream(raw)))
  }

  private def probeExcel(book: Workbook): Option[Savings.AssetValueHistory] = {
    // Rows 0 to 5 contain general information or nothing
    // Row 1 contains the investment fund name
    // Row 5 indicates which data are listed: cell 4 shall contain the date
    // and cell 5 the value at the given date
    // Rows 6 and beyond contain dated values
    val nameRowIdx = 1
    val labelsRowIdx = 5
    val dataRowIdx = labelsRowIdx + 1
    val dateCellIdx = 4
    val valueCellIdx = 5
    Some(book).filter { book =>
      // There should only be one sheet
      book.getNumberOfSheets == 1
    }.map { book =>
      book.getSheetAt(0)
    }.filter { sheet =>
      sheet.getSheetName.startsWith("Historique des valeurs liquida") &&
        (sheet.getFirstRowNum == 0) && (sheet.getLastRowNum >= dataRowIdx)
    }.flatMap { sheet =>
      val nameRow = sheet.getRow(nameRowIdx)
      val labelsRow = sheet.getRow(labelsRowIdx)

      if ((nameRow.getFirstCellNum == 0) && (nameRow.getLastCellNum >= 1) &&
        (nameRow.getCell(0).getStringCellValue == "Nom du fonds") &&
        (labelsRow.getFirstCellNum == 0) && (labelsRow.getLastCellNum >= math.max(dateCellIdx, valueCellIdx)) &&
        (labelsRow.getCell(dateCellIdx).getStringCellValue == "Date VL") &&
        (labelsRow.getCell(valueCellIdx).getStringCellValue == "VL"))
      {
        val dateFormatter = DateTimeFormatter.ofPattern("dd/MM/yyyy")
        val name = nameRow.getCell(1).getStringCellValue
        val values = (dataRowIdx to sheet.getLastRowNum).toList.map { rowIdx =>
          val row = sheet.getRow(rowIdx)
          Savings.AssetValue(
            date = LocalDate.parse(row.getCell(dateCellIdx).getStringCellValue, dateFormatter),
            value = row.getCell(valueCellIdx).getNumericCellValue
          )
        }
        Some(Savings.AssetValueHistory(Some(name), values))
      } else {
        None
      }
    }
  }

}
