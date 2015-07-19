package epsa

import java.nio.file.Path
import java.text.SimpleDateFormat

import org.apache.poi.ss.usermodel.WorkbookFactory

/**
 * Esalia support file prober.
 *
 * Probes whether a given file contains support information (name and asset
 * values) provided by Esalia site (excel file).
 */
object EsaliaSupportProber {

  def probe(path: Path): Option[Support] = {
    Option(WorkbookFactory.create(path.toFile)).filter { book =>
      // There should only be one sheet
      book.getNumberOfSheets == 1
    }.map { book =>
      book.getSheetAt(0)
    }.filter { sheet =>
      // Rows 0 to 5 contain general information
      // Rows 6 and beyond contain dated values
      sheet.getSheetName.startsWith("Historique des valeurs liquida") &&
        (sheet.getFirstRowNum == 0) && (sheet.getLastRowNum >= 6)
    }.flatMap { sheet =>
      // Row 1 contains the support name
      // Row 5 indicates which data are listed: cell 4 shall contain the date
      // and cell 5 the value at the given date
      val dateCellIdx = 4
      val valueCellIdx = 5
      val row1 = sheet.getRow(1)
      val row5 = sheet.getRow(5)

      if ((row1.getFirstCellNum == 0) && (row1.getLastCellNum >= 1) &&
        (row1.getCell(0).getStringCellValue == "Nom du fonds") &&
        (row5.getFirstCellNum == 0) && (row5.getLastCellNum >= 5) &&
        (row5.getCell(dateCellIdx).getStringCellValue == "Date VL") &&
        (row5.getCell(valueCellIdx).getStringCellValue == "VL"))
      {
        val dateParser = new SimpleDateFormat("dd/MM/yyyy")
        val name = row1.getCell(1).getStringCellValue
        val values = (6 to sheet.getLastRowNum).toList.map { rowIdx =>
          val row = sheet.getRow(rowIdx)
          AssetValue(
            date = dateParser.parse(row.getCell(dateCellIdx).getStringCellValue),
            value = row.getCell(valueCellIdx).getNumericCellValue
          )
        }
        Some(Support(name, values))
      } else {
        None
      }
    }
  }

}
