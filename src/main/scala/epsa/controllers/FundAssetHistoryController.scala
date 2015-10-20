package epsa.controllers

import javafx.scene.Scene
import javafx.stage.Stage

import epsa.I18N
import epsa.model.Savings
import java.util.{ResourceBundle, UUID}
import javafx.collections.FXCollections
import javafx.event.ActionEvent
import javafx.fxml.{FXMLLoader, FXML}
import javafx.scene.control.ComboBox
import javafx.scene.layout.VBox
import suiryc.scala.javafx.stage.Stages

import scala.collection.JavaConversions._
import suiryc.scala.javafx.util.Callback

class FundAssetHistoryController {

  @FXML
  protected var resources: ResourceBundle = _

  @FXML
  protected var vbox: VBox = _

  @FXML
  protected var fundField: ComboBox[Savings.Fund] = _

  //def initialize(): Unit = { }

  def initialize(savings: Savings, fundIdOpt: Option[UUID]): Unit = {
    // Note: we need to tell the combobox how to display both the 'button' area
    // (what is shown as selected) and the content (list of choices).
    fundField.setButtonCell(new FundCell)
    fundField.setCellFactory(Callback { new FundCell })

    // Scheme&fund with asset
    val funds = savings.schemes.flatMap { scheme =>
      scheme.funds.map(savings.getFund)
    }.distinct.sortBy(_.name)
    fundField.setItems(FXCollections.observableList(funds))
    fundIdOpt.flatMap { fundId =>
      funds.find(_.id == fundId)
    }.foreach(fundField.getSelectionModel.select)
  }

  def onSrcFund(event: ActionEvent): Unit = {
    Option(fundField.getValue).map(_.id) match {
      case Some(fundId) =>
      case None =>
    }
  }

}

object FundAssetHistoryController {

  /** Builds a stage out of this controller. */
  def buildStage(savings: Savings, fundId: Option[UUID]): Stage = {
    val resources = I18N.getResources

    val loader = new FXMLLoader(getClass.getResource("/fxml/fund-asset-history.fxml"), resources)
    val scene = new Scene(loader.load())
    val controller = loader.getController[FundAssetHistoryController]
    controller.initialize(savings, fundId)

    val stage = new Stage()
    val title = resources.getString("Fund asset history")
    stage.setTitle(title)
    stage.setScene(scene)

    Stages.trackMinimumDimensions(stage)

    stage
  }

}
