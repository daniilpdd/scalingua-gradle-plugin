package pro.sgaz

package object scalingua {
  import java.io.File

  implicit class RichFile(asFile: File) {
    def hash: Array[Byte] = Hash(asFile)
    def hashString: String = Hash.toHex(hash)
    def /(component: String): File = if (component == ".") asFile else new File(asFile, component)
  }
}
