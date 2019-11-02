package epsa

import akka.actor.ActorSystem
import epsa.controllers.MainController
import epsa.model.Savings
import epsa.util.Awaits
import java.nio.file.Path
import javafx.stage.Stage
import monix.execution.Scheduler
import suiryc.scala.Configuration
import scala.concurrent.ExecutionContextExecutor
import suiryc.scala.akka.CoreSystem
import suiryc.scala.javafx.{JFXApplication, JFXLauncher}
import suiryc.scala.javafx.concurrent.JFXSystem
import suiryc.scala.log.Loggers
import suiryc.scala.misc.Util

object Main extends JFXLauncher[MainApp] {

  Configuration.setDefaultApplication()

  import Settings.Debug

  val appPath: Path = Util.classLocation[this.type]

  lazy val settings = new Settings(appPath.resolve("application.conf"))

  val versionedName: String = s"${epsa.Info.name} ${epsa.Info.version}" +
    epsa.Info.gitHeadCommit.map(v => s" ($v)").getOrElse("")

  override def main(args: Array[String]): Unit = {
    val parser = new scopt.OptionParser[Params](getClass.getCanonicalName) {
      head(versionedName)
      help("help")
      opt[String]("debug").unbounded.foreach { v =>
        Settings.debugParams ++= v.split(',').toList.map(Debug.withName).toSet
      }
      opt[Boolean]("io-capture").action { (v, c) =>
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
      case Some(params) =>
        // Redirect stdout/stderr to logs.
        // Note: scala 'Console' stores the current 'in/out/err' value. So
        // better not trigger it before redirecting streams. (methods to change
        // the values are marked deprecated)
        if (params.ioCapture.contains(true)) Loggers.captureIo()
        // 'launch' does not return until application is closed
        super.main(args)

      case None =>
        sys.exit(1)
    }
  }

  object Akka {

    implicit val system: ActorSystem = CoreSystem.system
    implicit val dispatcher: ExecutionContextExecutor = system.dispatcher

  }

  val scheduler: Scheduler = CoreSystem.scheduler

  override def shutdown(): Unit = {
    JFXSystem.terminate()
    ()
  }

  case class Params(
    ioCapture: Option[Boolean] = Some(true)
  )

}

class MainApp extends JFXApplication {

  override def start(stage: Stage): Unit = {
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
