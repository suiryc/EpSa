package epsa.tools

import epsa.model.Savings
import java.nio.file.Path
import java.time.{LocalDate, ZoneId}
import java.time.format.DateTimeFormatter
import org.apache.poi.ss.usermodel.WorkbookFactory
import org.odftoolkit.simple.SpreadsheetDocument
import suiryc.scala.io.PathsEx

/**
 * Minimal spreadsheet investment fund file prober.
 *
 * Probes whether a given file contains one sheet with at least two columns
 * (date and value) interpreted as investment fund information.
 * Works with Excel (xls/xlsx) and OpenDocument (ods) files.
 */
object SpreadsheetInvestmentFundProber extends InvestmentFundProber {

  override def probe(path: Path): Option[Savings.AssetValueHistory] = {
    val extension = PathsEx.extension(path).toLowerCase
    try {
      if ((extension == "xls") || (extension == "xlsx")) {
        probeExcel(path)
      } else if (extension == "ods") {
        probeOpenDocument(path)
      } else None
    } catch {
      case ex: Exception =>
        ex.printStackTrace()
        None
    }
  }

  private def probeExcel(path: Path): Option[Savings.AssetValueHistory] = {
    Some(WorkbookFactory.create(path.toFile)).filter { book =>
      // There should only be one sheet
      book.getNumberOfSheets == 1
    }.map { book =>
      book.getSheetAt(0)
    }.filter { sheet =>
      // There should ba at least one row
      (sheet.getFirstRowNum == 0) && (sheet.getLastRowNum >= 0)
    }.map { sheet =>
      // Cell 0 contains the date and cell 1 the value at the given date
      val dateCellIdx = 0
      val valueCellIdx = 1

      val dateFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd")
      val values = (0 to sheet.getLastRowNum).toList.map { rowIdx =>
        val row = sheet.getRow(rowIdx)
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
      }
      Savings.AssetValueHistory(None, values)
    }
  }

  private def probeOpenDocument(path: Path): Option[Savings.AssetValueHistory] = {
    Some(SpreadsheetDocument.loadDocument(path.toFile)).filter { doc =>
      // There should only be one sheet
      doc.getSheetCount == 1
    }.map { doc =>
      doc.getSheetByIndex(0)
    }.filter { table =>
      // There should ba at least one row, and at least 2 columns
      (table.getRowCount > 0) && (table.getColumnCount >= 2)
    }.map { table =>
      import scala.collection.JavaConversions._
      // Cell 0 contains the date and cell 1 the value at the given date
      val dateCellIdx = 0
      val valueCellIdx = 1

      val dateFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd")
      val values = table.getRowList.toList.map { row =>
        val date = try {
          // Try as a date
          row.getCellByIndex(dateCellIdx).getDateValue.toInstant.atZone(ZoneId.systemDefault).toLocalDate
        } catch {
          case _: Exception =>
            // Fallback to test parsing
            LocalDate.parse(row.getCellByIndex(dateCellIdx).getStringValue, dateFormatter)
        }
        Savings.AssetValue(
          date = date,
          value = row.getCellByIndex(valueCellIdx).getDoubleValue.doubleValue
        )
      }
      Savings.AssetValueHistory(None, values)
    }
  }

}