package epsa.storage

import java.io.File
import scalikejdbc._

object DataStore {

  // See: http://stackoverflow.com/a/12733172

  val pathDefault = {
    val file = new File(getClass.getProtectionDomain.getCodeSource.getLocation.toURI)
    if (file.isDirectory) file
    else file.getParent
  }

  GlobalSettings.loggingSQLAndTime = LoggingSQLAndTimeSettings(
    enabled = true,
    singleLineMode = false,
    printUnprocessedStackTrace = false,
    stackTraceDepth= 15,
    logLevel = 'debug,
    warningEnabled = false,
    warningThresholdMillis = 3000L,
    warningLogLevel = 'warn
  )

  Class.forName("org.h2.Driver")
  ConnectionPool.singleton(s"jdbc:h2:${pathDefault}/default", "user", "pass")

  implicit val session = AutoSession

  sql"""
create table members (
  id serial not null primary key,
  name varchar(64),
  created_at timestamp not null
)
""".execute.apply()

  Seq("Alice", "Bob", "Chris") foreach { name =>
    sql"insert into members (name, created_at) values (${name}, current_timestamp)".update.apply()
  }

  val entities: List[Map[String, Any]] = sql"select * from members".map(_.toMap).list.apply()

  println(entities)

  import org.joda.time._
  case class Member(id: Long, name: Option[String], createdAt: DateTime)
  object Member extends SQLSyntaxSupport[Member] {
    override val tableName = "members"
    def apply(rs: WrappedResultSet) = new Member(
      rs.long("id"), rs.stringOpt("name"), rs.jodaDateTime("created_at"))
  }

  val members: List[Member] = sql"select * from members".map(rs => Member(rs)).list.apply()

  println(members)

  val m = Member.syntax("m")
  val name = "Alice"
  val alice: Option[Member] = withSQL {
    select.from(Member as m).where.eq(m.name, name)
  }.map(rs => Member(rs)).single.apply()

  println(alice)

}
