package epsa

import akka.actor.ActorSystem
import epsa.controllers.MainController
import epsa.model.Savings
import epsa.util.Awaits
import javafx.application.{Application, Platform}
import javafx.stage.Stage
import monix.execution.Scheduler
import scala.concurrent.ExecutionContextExecutor
import suiryc.scala.akka.CoreSystem

object Main {

  import Settings.Debug

  val versionedName: String = s"${epsa.Info.name} ${epsa.Info.version}" +
    epsa.Info.gitHeadCommit.map(v => s" ($v)").getOrElse("")

  def main(args: Array[String]): Unit = {
    val parser = new scopt.OptionParser[Unit](getClass.getCanonicalName) {
      head(versionedName)
      help("help")
      opt[String]("debug").unbounded.foreach { v =>
        Settings.debugParams ++= v.split(',').toList.map(Debug.withName).toSet
      }
      opt[Unit]("version").foreach { _ =>
        println(
          s"""$versionedName
             |scalaVersion: ${Info.scalaVersion}
             |sbtVersion: ${Info.sbtVersion}
           """.stripMargin)
        sys.exit(0)
      }
    }

    if (parser.parse(args)) (new Main).launch()
  }

  object Akka {

    implicit val system: ActorSystem = CoreSystem.system
    implicit val dispatcher: ExecutionContextExecutor = system.dispatcher
    val scheduler: Scheduler = CoreSystem.scheduler

  }

  def shutdown(stage: Stage): Unit = {
    stage.close()
    shutdown()
  }

  def shutdown(): Unit = {
    Akka.system.terminate()
    Platform.exit()
  }

}

class Main extends Application {

  def launch(): Unit = {
    Application.launch()
  }

  override def start(stage: Stage) {
    I18N.loadLocale()

    def startController(): Unit = {
      // Note: actual state (account events) will be built by controller.
      val savings = Savings()
      val state = MainController.State(
        stage = stage,
        savings = savings
      )
      MainController.build(state, applicationStart = true)
    }

    // Note: if stage has no Scene, have it owns a Dialog fails.
    // In any case, we have yet to build and show the stage.
    Awaits.openDataStore(None)
    // If we failed to read events, user was warned.
    startController()
  }

}
