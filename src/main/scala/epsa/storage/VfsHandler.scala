package epsa.storage

import com.typesafe.scalalogging.StrictLogging
import java.nio.ByteBuffer
import java.nio.file.Path
import org.h2.store.fs.FileUtils
import org.h2.store.fs.FilePath
import scala.collection.JavaConverters._
import suiryc.scala.util.Using

object VfsHandler extends StrictLogging {

  private val VFS_SCHEME = "memFS"
  private val PATH_VIRTUAL = s"$VFS_SCHEME:/virtual/"

  private val pathVirtual = FilePath.get(PATH_VIRTUAL)

  /** Deletes and recreates the virtual path. */
  def cleanup(): Unit = {
    FileUtils.deleteRecursive(pathVirtual.toString, false)
    pathVirtual.createDirectory()
  }

  cleanup()

  /**
   * Virtualizes given path.
   *
   * Virtual path is cleaned up, then given file content is copied into it (with
   * same filename).
   * Caller is expected to ensure that nothing is using the virtual path.
   */
  def virtualize(path: Path): FilePath = {
    cleanup()
    val src = FilePath.get(path.toString)
    val dst = FilePath.get(s"$PATH_VIRTUAL${path.getFileName.toString}")
    transfer(src, dst)
    dst
  }

  /**
   * Synchronizes the virtual path into the target path.
   *
   * All files present in virtual folder are copied into target path.
   */
  def sync(path: Path): Unit = {
    FileUtils.createDirectories(path.toString)
    pathVirtual.newDirectoryStream.asScala.foreach { filepath ⇒
      if (filepath.isDirectory) {
        logger.warn(s"Found directory path=<$filepath> inside virtual space")
      } else {
        transfer(filepath, FilePath.get(path.resolve(filepath.getName).toString))
      }
    }
  }

  /** Copies file content. */
  def transfer(src: FilePath, dst: FilePath): Unit = {
    Using.Manager { use ⇒
      val bb = ByteBuffer.allocate(512 * 1024)
      val in = use(src.open("r"))
      val out = use(dst.open("rw"))
      out.truncate(0)

      @scala.annotation.tailrec
      def fill(): Unit = if (bb.hasRemaining && (in.read(bb) >= 0)) fill()

      @scala.annotation.tailrec
      def flush(): Unit = if (bb.hasRemaining) {
        out.write(bb)
        flush()
      }

      @scala.annotation.tailrec
      def loop(): Unit = {
        bb.clear()
        fill()
        val eof = bb.hasRemaining
        bb.flip()
        flush()
        if (!eof) loop()
      }

      loop()
    }.get
  }

}
