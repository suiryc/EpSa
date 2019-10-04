package epsa.tools

import epsa.model.Savings
import java.nio.file.Path
import java.time.{LocalDate, ZoneId}
import java.time.format.DateTimeFormatter
import org.apache.poi.ss.usermodel.{CellType, WorkbookFactory}
import suiryc.scala.io.PathsEx

/**
 * BNP Paribas investment fund file prober.
 *
 * Probes whether a given file contains investment fund information (name and
 * asset values) provided by BNP Paribas site (excel file).
 */
object BNPParibasInvestmentFundProber extends InvestmentFundProber {

  override def probe(path: Path): Option[Savings.AssetValueHistory] = {
    val extension = PathsEx.extension(path).toLowerCase
    try {
      if ((extension == "xls") || (extension == "xlsx")) {
        probeExcel(path)
      } else None
    } catch {
      case ex: Exception =>
        ex.printStackTrace()
        None
    }
  }

  private def probeExcel(path: Path): Option[Savings.AssetValueHistory] = {
    // Rows 0 to 14 contain general information or nothing
    // Row 7 contains the investment fund name
    // Row 14 indicates which data are listed: cell 0 shall contain the date
    // and cell 1 the value at the given date
    // Rows 15 and beyond contain dated values
    val nameRowIdx = 7
    val labelsRowIdx = 14
    val dataRowIdx = labelsRowIdx + 1
    val dateCellIdx = 0
    val valueCellIdx = 1
    Some(WorkbookFactory.create(path.toFile)).filter { book =>
      // There should only be one sheet
      book.getNumberOfSheets == 1
    }.map { book =>
      book.getSheetAt(0)
    }.filter { sheet =>
      (sheet.getSheetName == "Sheet0") &&
        (sheet.getFirstRowNum == 0) && (sheet.getLastRowNum >= dataRowIdx)
    }.flatMap { sheet =>
      val nameRow = sheet.getRow(nameRowIdx)
      val labelsRow = sheet.getRow(labelsRowIdx)

      if ((nameRow.getFirstCellNum == 0) && (nameRow.getLastCellNum >= 1) &&
        (nameRow.getCell(0).getStringCellValue == "Nom") &&
        (labelsRow.getFirstCellNum == 0) && (labelsRow.getLastCellNum >= math.max(dateCellIdx, valueCellIdx)) &&
        (labelsRow.getCell(dateCellIdx).getStringCellValue == "Date") &&
        (labelsRow.getCell(valueCellIdx).getStringCellValue == "Dernière VL"))
      {
        val dateFormatter = DateTimeFormatter.ofPattern("dd/MM/yyyy")
        val name = nameRow.getCell(1).getStringCellValue
        // Table is ended by an empty row (while last row contains a warning comment)
        val values = (dataRowIdx to sheet.getLastRowNum).to(LazyList).map(sheet.getRow).takeWhile { row =>
          // Notes:
          // Cell.getCellType was made deprecated prior to being changed between
          // 3.x and 4.x to return an enum instead of an int. In the meantime
          // (starting with 3.15) the temporary getCellTypeEnum function was to
          // be used, and is deprecated in 4.x (will be removed in 4.2).
          row.getCell(dateCellIdx).getCellType != CellType.BLANK
        }.map { row =>
          val date = try {
            // Try as a date
            row.getCell(dateCellIdx).getDateCellValue.toInstant.atZone(ZoneId.systemDefault).toLocalDate
          } catch {
            case _: Exception =>
              // Fallback to test parsing
              LocalDate.parse(row.getCell(dateCellIdx).getStringCellValue, dateFormatter)
          }
          Savings.AssetValue(
            date = date,
            value = row.getCell(valueCellIdx).getNumericCellValue
          )
        }.toList
        Some(Savings.AssetValueHistory(Some(name), values))
      } else {
        None
      }
    }
  }

}
