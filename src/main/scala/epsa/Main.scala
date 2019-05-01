package epsa

import akka.actor.ActorSystem
import epsa.controllers.MainController
import epsa.model.Savings
import epsa.util.Awaits
import java.nio.file.Path
import javafx.application.Application
import javafx.stage.Stage
import monix.execution.Scheduler
import scala.concurrent.ExecutionContextExecutor
import suiryc.scala.akka.CoreSystem
import suiryc.scala.io.SystemStreams
import suiryc.scala.javafx.concurrent.JFXSystem
import suiryc.scala.misc.Util

object Main {

  import Settings.Debug

  val appPath: Path = Util.classLocation[this.type]

  lazy val settings = new Settings(appPath.resolve("application.conf"))

  val versionedName: String = s"${epsa.Info.name} ${epsa.Info.version}" +
    epsa.Info.gitHeadCommit.map(v => s" ($v)").getOrElse("")

  def main(args: Array[String]): Unit = {
    val parser = new scopt.OptionParser[Params](getClass.getCanonicalName) {
      head(versionedName)
      help("help")
      opt[String]("debug").unbounded.foreach { v =>
        Settings.debugParams ++= v.split(',').toList.map(Debug.withName).toSet
      }
      opt[Boolean]("io-capture").action { (v, c) ⇒
        c.copy(ioCapture = Some(v))
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

    parser.parse(args, Params()) match {
      case Some(params) ⇒
        // Redirect stdout/stderr to logs.
        // Note: scala 'Console' stores the current 'in/out/err' value. So
        // better not trigger it before redirecting streams. (methods to change
        // the values are marked deprecated)
        val ioCapture = params.ioCapture.contains(true)
        if (ioCapture) {
          SystemStreams.replace(
            SystemStreams.loggerOutput("stdout"),
            SystemStreams.loggerOutput("stderr", error = true)
          )
        }
        // 'launch' does not return until application is closed
        Application.launch(classOf[Main])

      case None ⇒
        sys.exit(1)
    }
  }

  object Akka {

    implicit val system: ActorSystem = CoreSystem.system
    implicit val dispatcher: ExecutionContextExecutor = system.dispatcher

  }

  val scheduler: Scheduler = CoreSystem.scheduler

  def shutdown(stage: Stage): Unit = {
    stage.close()
    shutdown()
  }

  def shutdown(): Unit = {
    JFXSystem.terminate()
    ()
  }

  case class Params(
    ioCapture: Option[Boolean] = Some(true)
  )

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
