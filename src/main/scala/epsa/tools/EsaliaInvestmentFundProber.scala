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
    // Notes:
    // There should be at least one sheet.
    // The first one is named "Historique des VL". Usually there is a second
    // empty sheet.
    //
    // In all rows, cell 0 is empty.
    // Rows 0 to 4 contain nothing.
    // Rows 5 to 13 contain information on fund and extraction request:
    //  - row 7 is for the investment fund name: cell 1 contains "Nom du fonds"
    //    while cell 2 contains the fund name
    // Row 14 is empty.
    // Row 15 indicates which data are listed in rows 16 and beyond.
    // In row 15, cell 1 contains "Dates". Initially cell 2 contained "VL(C)",
    // and lately "VL(data.result_assignment.capitalisation)".
    // Dated values are listed in reverse history: latest/earliest date first.
    val firstRowIdx = 5
    val firstCellIdx = 1
    val nameRowIdx = 7
    val labelsRowIdx = 15
    val dataRowIdx = labelsRowIdx + 1
    val dateCellIdx = 1
    val valueCellIdx = 2
    Some(book).filter { book =>
      // There must be at least one sheet
      book.getNumberOfSheets >= 1
    }.map { book =>
      book.getSheetAt(0)
    }.filter { sheet =>
      sheet.getSheetName.startsWith("Historique des VL") &&
        (sheet.getFirstRowNum == firstRowIdx) && (sheet.getLastRowNum >= dataRowIdx)
    }.flatMap { sheet =>
      val nameRow = sheet.getRow(nameRowIdx)
      val labelsRow = sheet.getRow(labelsRowIdx)

      if ((nameRow.getFirstCellNum == firstCellIdx) && (nameRow.getLastCellNum > firstCellIdx) &&
        (nameRow.getCell(firstCellIdx).getStringCellValue == "Nom du fonds") &&
        (labelsRow.getFirstCellNum == firstCellIdx) && (labelsRow.getLastCellNum >= math.max(dateCellIdx, valueCellIdx)) &&
        (labelsRow.getCell(dateCellIdx).getStringCellValue == "Dates") &&
        labelsRow.getCell(valueCellIdx).getStringCellValue.startsWith("VL"))
      {
        val dateFormatter = DateTimeFormatter.ofPattern("dd/MM/yyyy")
        val name = nameRow.getCell(firstCellIdx + 1).getStringCellValue
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
