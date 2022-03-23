package pro.sgaz.scalingua

import org.gradle.api.provider.Property
import org.gradle.api.{Plugin, Project, Task}
import ru.makkarpov.scalingua.LanguageId

import java.io.File
import java.nio.file.{Files, Paths}

trait ScalinguaSettingsExtension {
  def getTemplateTarget: Property[String]
  def getLocalePackage: Property[String]
  def getCompileLocalesStrategy: Property[String]
  def getEscapeUnicode: Property[Boolean]
}

case class ProjectSettings(templateTa)

class ScalinguaPlugin extends Plugin[Project] {
  private def longestCommonPrefix(s: Seq[File]): String = s match {
    case Seq() => ""
    case Seq(head, tail@_*) =>
      var current = head.getCanonicalPath
      for (f <- tail)
        current = current.zip(f.getCanonicalPath).takeWhile { case (a, b) => a == b }.map(_._1).mkString
      current
  }

  private def createParent(f: File): Unit = {
    val par = f.getParentFile
    if ((par ne null) && !par.isDirectory)
      Files.createDirectory(Paths.get(par.getPath))
  }

  private def filePkg(f: File, s: String): File =
    if (s.isEmpty) f
    else f / s.replace('.', '/')

  def collectLangs(task: Seq[File]): Seq[LanguageId] = {
    val langPattern = "^([a-z]{2})_([A-Z]{2})\\.po$".r
    val ret = Seq.newBuilder[LanguageId]

    for (src <- task) src.getName match {
      case langPattern(language, country) =>
        ret += LanguageId(language, country)

      case _ =>
        throw new IllegalArgumentException(s"Illegal file name '${src.getName}', should be formatted like 'en_US.po' (${src.getCanonicalPath})")
    }

    ret.result()
  }

  def compileLocalesTask(implicit extension: ScalinguaSettingsExtension): Seq[File] = {
    val strategy = PoCompilerStrategy.getStrategy(extension.getCompileLocalesStrategy.get())

    val doCompiling: GenerationContext => Unit = PoCompiler.doCompiling(strategy)
    val compileEnglishTags: GenerationContext => Unit = PoCompiler.compileEnglishTags(strategy)

    val r = withGenContext(
      compileLocales,
      "Language_%(l)_%(c).scala",
      "CompiledEnglishTags.scala")(
      perLang = doCompiling,
      englishTags = compileEnglishTags)

    if (strategy.generatesIndex) {
      val idx = {
        val langs = collectLangs(compileLocales).value
        val pkg = (localePackage in compileLocales).value

        val tgt = filePkg((target in compileLocales).value, pkg) / "Languages.scala"
        createParent(tgt)

        PoCompiler.generateIndex(pkg, tgt, langs, (taggedFile in compileLocales).value.isDefined)

        tgt
      }

      r :+ idx
    } else
      r
  }

  def withGenContext(task: Seq[File], langFormat: String, tagFormat: String)
                    (perLang: GenerationContext => Unit, englishTags: GenerationContext => Unit): Seq[File] = {
    val baseTgt = (target in task).value
    val pkg = (localePackage in task).value
    val implicitCtx =
      if ((includeImplicitContext in task).value) (implicitContext in task).value.filter(_.nonEmpty)
      else None
    val log = getLogger
    val hasTags = (taggedFile in task).value.isDefined

    val langPattern = "^([a-z]{2})_([A-Z]{2})\\.po$".r
    val ret = Seq.newBuilder[File]

    for (src <- (sources in task).value) src.getName match {
      case langPattern(language, country) =>
        val tgt = filePkg(baseTgt, pkg) / StringUtils.interpolate(langFormat, "l" -> language, "c" -> country)
        createParent(tgt)

        val genCtx = GenerationContext(pkg, implicitCtx, LanguageId(language, country), hasTags, src, tgt, log)
        try perLang(genCtx)
        catch {
          case p: ParseFailedException =>
            throw p

          case t: Throwable =>
            throw new IllegalArgumentException(s"Failed to compile ${src.getCanonicalPath}", t)
        }

        ret += tgt
      case _ =>
        throw new IllegalArgumentException(s"Illegal file name '${src.getName}', should be formatted like 'en_US.po' (${src.getCanonicalPath})")
    }

    for (t <- (taggedFile in task).value) {
      val tgt = filePkg(baseTgt, pkg) / tagFormat

      val genCtx = GenerationContext(pkg, implicitCtx, LanguageId("xx", "XX"), hasTags, t, tgt, log)
      englishTags(genCtx)
      ret += tgt
    }

    ret.result()
  }

  override def apply(project: Project): Unit = {

    implicit val extension: ScalinguaSettingsExtension = project.getExtensions.create[ScalinguaSettingsExtension]("scalingua", classOf[ScalinguaSettingsExtension])

    val localePackage = "locales"
    val compileLocalesStrategy = "ReadFromResources"
    val escapeUnicode = true

    val settings = ProjectSettings()

    project.task("compileLocales").doLast{ _: Task =>
      println(s"templateTarget: ${extension.getTemplateTarget.get()}")
      println(s"localePackage: ${extension.getLocalePackage.get()}")
      println(s"compileLocalesStrategy: ${extension.getCompileLocalesStrategy.get()}")
      println(s"escapeUnicode: ${extension.getEscapeUnicode.get()}")
    }

    project.task("packageLocales").doLast{ _: Task =>
      val strategy = PoCompilerStrategy.getStrategy(extension.getCompileLocalesStrategy.get())

//      if (strategy.isPackagingNecessary)
      println(strategy)

    }
  }
}
