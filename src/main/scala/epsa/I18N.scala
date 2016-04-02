package epsa

import suiryc.scala.util.{I18NWithCache, I18NWithPreference}

object I18N extends suiryc.scala.util.I18N("i18n.epsa") with I18NWithPreference with I18NWithCache {

  import epsa.Settings.prefs
  import suiryc.scala.settings.Preference
  import suiryc.scala.settings.Preference._

  override val pref = Preference.from("locale.code", "en")

  object Strings {
    // Titles, menu items, actions, ...
    def editSchemes = getString("Edit schemes")
    def editScheme = getString("Edit scheme")
    def editFunds = getString("Edit funds")
    def editFund = getString("Edit fund")
    def newPayment = getString("New payment")
    def newTransfer = getString("New transfer")
    def newRefund = getString("New refund")
    def empty = getString("Empty")
    def navHistory = getString("Net asset value history")
    def importNAVHistory = getString("Import net asset value history")
    def purgeNAVHistory = getString("Purge net asset value history")
    def options = getString("Options")
    def selectDataStore = getString("Select data store")

    // Labels, ...
    def na = getString("n/a")
    def available = getString("available")
    def scheme = getString("Scheme")
    def schemeColon = getString("Scheme:")
    def fund = getString("Fund")
    def fundColon = getString("Fund:")
    def availability = getString("Availability")
    def availabilityColon = getString("Availability:")
    def units = getString("Units")
    def unitsColon = getString("Units:")
    def vwap = getString("VWAP")
    def vwapColon = getString("VWAP:")
    def investedLFAmount = getString("Invested\namount")
    def investedAmountColon = getString("Invested amount:")
    def date = getString("Date")
    def dateColon = getString("Date:")
    def nav = getString("NAV")
    def navColon = getString("NAV:")
    def amount = getString("Amount")
    def amountColon = getString("Amount:")
    def payment = getString("Payment")
    def transfer = getString("Transfer")
    def refund = getString("Refund")
    def oldValue = getString("Old value")
    def newValue = getString("New value")
    def spreadsheets = getString("Spreadsheets")
    def dataStore = getString("Data store")

    // Messages (information, warning, error), ...
    def pendingChanges = getString("confirmation.pending-changes")
    def confirmAction = getString("confirmation.action")
    def irreversibleAction = getString("confirmation.irreversible-action")
    def undoPending = getString("Undo all pending changes")
    def needRestart = getString("information.need-restart")
    def mandatoryField = getString("Mandatory field")
    def positiveValue = getString("Positive value expected")
    def valueLimit = getString("Value exceeds available quantity")
    def anteriorOpDate = getString("warning.anterior-operation-date")
    def anteriorAvailDate = getString("error.anterior-availability-date")
    def dstAmountDelta = getString("warning.dst-amount-delta")
    def unselectingNonEmptyResource = getString("warning.unselecting-nonempty-resource")
    def deleteAssociatedSchemes = getString("confirmation.delete-associated-schemes")
    def deleteAssociatedFunds = getString("confirmation.delete-associated-funds")
    def nameExists = getString("Name already exists")
    def nameEmpty = getString("Name cannot be empty")
    def schemeNotEmpty =getString("Scheme is not empty")
    def fundNotEmpty = getString("Fund is not empty")
    def unhandledResource = getString("warning.unhandled-resource")
    def unknownFormat = getString("Unknown format")
    def noDataStoreSelected = getString("No data store selected")
    def dataStoreWriteError = getString("Could not write data store")
    def dataStoreReadError = getString("Could not read data store")
    def dataStoreCleaned = getString("Cleaned up data store")
    def dataStoreCleanError = getString("Could not cleanup data store")
  }

}
