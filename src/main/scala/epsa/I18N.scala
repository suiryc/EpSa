package epsa

import suiryc.scala.util.{I18NWithCache, I18NWithPreference}

object I18N extends suiryc.scala.util.I18N("i18n.epsa") with I18NWithPreference with I18NWithCache {

  import epsa.Settings.prefs
  import suiryc.scala.settings.Preference
  import suiryc.scala.settings.Preference._

  override val pref: Preference[String] = Preference.from(prefs, "locale.code", "en")

  object Strings {
    // Titles, menu items, actions, ...
    def editSchemes: String = getString("Edit schemes")
    def editScheme: String = getString("Edit scheme")
    def editFunds: String = getString("Edit funds")
    def editFund: String = getString("Edit fund")
    def editUnavailabilityPeriods: String = getString("Edit unavailability periods")
    def newPayment: String = getString("New payment")
    def newTransfer: String = getString("New transfer")
    def newRefund: String = getString("New refund")
    def savings: String = getString("Savings")
    def savingsOnDate: String = getString("savings-on-date")
    def savingsOnDateTab: String = getString("savings-on-date.tab")
    def accountHistory: String = getString("Account history")
    def navHistory: String = getString("Net asset value history")
    def levies: String = getString("Levies")
    def importLevies: String = getString("Import levies")
    def importNAVHistory: String = getString("Import net asset value history")
    def purgeNAVHistory: String = getString("Purge net asset value history")
    def options: String = getString("Options")
    def selectDataStore: String = getString("Select data store")
    def exportRawAccountHistory: String = getString("Export raw account history")
    def importRawAccountHistory: String = getString("Import raw account history")

    // Labels, ...
    def na: String = getString("n/a")
    def available: String = getString("available")
    def scheme: String = getString("Scheme")
    def schemeColon: String = getString("Scheme:")
    def fund: String = getString("Fund")
    def fundColon: String = getString("Fund:")
    def availability: String = getString("Availability")
    def availabilityColon: String = getString("Availability:")
    def units: String = getString("Units")
    def unitsColon: String = getString("Units:")
    def vwap: String = getString("VWAP")
    def vwapColon: String = getString("VWAP:")
    def date: String = getString("Date")
    def dateColon: String = getString("Date:")
    def nav: String = getString("NAV")
    def navColon: String = getString("NAV:")
    def amount: String = getString("Amount")
    def invested: String = getString("Invested")
    def investedAmountColon: String = getString("Invested amount:")
    def gross: String = getString("Gross")
    def grossAmount: String = getString("Gross amount")
    def grossAmountColon: String = getString("Gross amount:")
    def leviesAmountColon: String = getString("Levies amount:")
    def net: String = getString("Net")
    def netAmountColon: String = getString("Net amount:")
    def gain: String = getString("Gain/Loss")
    def grossGainColon: String = getString("Gross gain/loss:")
    def grossPct: String = getString("Gross (%)")
    def grossGainPctColon: String = getString("Gross gain/loss (%):")
    def netGainColon: String = getString("Net gain/loss:")
    def netPct: String = getString("Net (%)")
    def netGainPctColon: String = getString("Net gain/loss (%):")
    def payment: String = getString("Payment")
    def transfer: String = getString("Transfer")
    def refund: String = getString("Refund")
    def oldValue: String = getString("Old value")
    def newValue: String = getString("New value")
    def spreadsheets: String = getString("Spreadsheets")
    def dataStore: String = getString("Data store")
    def event: String = getString("Event")
    def jsonFiles: String = getString("JSON files")

    // Messages (information, warning, error), ...
    def assetEventPaymentMain: String = getString("asset.event.payment.main")
    def assetEventPaymentDetails1: String = getString("asset.event.payment.details.1")
    def assetEventTransferMain: String = getString("asset.event.transfer.main")
    def assetEventTransferDetails1: String = getString("asset.event.transfer.details.1")
    def assetEventTransferDetails2: String = getString("asset.event.transfer.details.2")
    def assetEventRefundMain: String = getString("asset.event.refund.main")
    def assetEventRefundDetails1: String = getString("asset.event.refund.details.1")
    def assetEventSchemeRenamed: String = getString("asset.event.scheme.renamed")
    def assetEventFundRenamed: String = getString("asset.event.fund.renamed")
    def leviesEstimation: String = getString("levies.estimation")
    def pendingAction: String = getString("confirmation.pending-action")
    def pendingChanges: String = getString("confirmation.pending-changes")
    def confirmAction: String = getString("confirmation.action")
    def irreversibleAction: String = getString("confirmation.irreversible-action")
    def undoPending: String = getString("Undo all pending changes")
    def needRestart: String = getString("information.need-restart")
    def mandatoryField: String = getString("Mandatory field")
    def positiveValue: String = getString("Positive value expected")
    def valueLimit: String = getString("Value exceeds available quantity")
    def anteriorOpDate: String = getString("warning.anterior-operation-date")
    def anteriorAvailDate: String = getString("error.anterior-availability-date")
    def dstAmountDelta: String = getString("warning.dst-amount-delta")
    def unselectingNonEmptyResource: String = getString("warning.unselecting-nonempty-resource")
    def deleteAssociatedSchemes: String = getString("confirmation.delete-associated-schemes")
    def deleteAssociatedFunds: String = getString("confirmation.delete-associated-funds")
    def deleteUsedResource: String = getString("confirmation.delete-used-resource")
    def nameExists: String = getString("Name already exists")
    def nameEmpty: String = getString("Name cannot be empty")
    def schemeNotEmpty: String =getString("Scheme is not empty")
    def fundNotEmpty: String = getString("Fund is not empty")
    def unhandledResource: String = getString("warning.unhandled-resource")
    def unknownFormat: String = getString("Unknown format")
    def noDataStoreSelected: String = getString("No data store selected")
    def dataStoreWriteError: String = getString("Could not write data store")
    def dataStoreReadError: String = getString("Could not read data store")
    def dataStoreEventsReordered: String = getString("information.datastore-events-reordered")
    def dataStoreCleaned: String = getString("Cleaned up data store")
    def dataStoreCleanError: String = getString("Could not cleanup data store")
    def fileReadError: String = getString("Could not read file")
    def fileWriteError: String = getString("Could not write file")
    def accountHistoryIssues: String = getString("warning.account-history.issues")
    def accountHistoryIssuesNAV: String = getString("warning.account-history.issues.nav")
    def downloadNavHistoryError: String = getString("error.download-nav-history")
    def unexpectedIssue: String = getString("error.unexpected-issue")
  }

}
