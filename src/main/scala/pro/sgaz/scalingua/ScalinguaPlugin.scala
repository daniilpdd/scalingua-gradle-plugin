package pro.sgaz.scalingua

import org.gradle.api.provider.Property
import org.gradle.api.tasks.scala.ScalaCompile
import org.gradle.api.{Plugin, Project, Task}
import org.slf4j.{Logger, LoggerFactory}
import ru.makkarpov.scalingua.{LanguageId, StringUtils}

import java.io.File
import scala.collection.JavaConverters.{asScalaBufferConverter, seqAsJavaListConverter}

trait ScalinguaSettingsExtension {
  def getLocalePackage: Property[String]
  def getCompileLocalesStrategy: Property[String]
  def getTaggetFile: Property[String]
  def getSrcPackage: Property[String]
}

case class ProjectSettings(
                            compileLocalesSettings: CompileLocalesSettings,
                            packageLocalesSettings: PackageLocalesSettings,
                            compileLocalesStrategy: String,
                            templateTarget: String,
                            srcName: String
                          )

sealed trait TaskSettings {
  val target: File
  val localePackage: String
  val sources: Seq[File]
  val taggetFile: Option[File]
}

case class CompileLocalesSettings(target: File, localePackage: String, sources: Seq[File], taggetFile: Option[File]) extends TaskSettings

case class PackageLocalesSettings(target: File, localePackage: String, sources: Seq[File], taggetFile: Option[File]) extends TaskSettings

class ScalinguaPlugin extends Plugin[Project] {
  private val logger: Logger = LoggerFactory.getLogger(getClass)

  private def createParent(f: File): Unit = {
    val par = f.getParentFile
    if ((par ne null) && !par.isDirectory)
      par.mkdirs()
  }

  private def filePkg(f: File, s: String): File =
    if (s.isEmpty) f
    else f / s.replace('.', '/')

  private def collectLangs(task: TaskSettings): Seq[LanguageId] = {
    val langPattern = "^([a-z]{2})_([A-Z]{2})\\.po$".r
    val ret = Seq.newBuilder[LanguageId]

    for (src <- task.sources) src.getName match {
      case langPattern(language, country) =>
        ret += LanguageId(language, country)

      case _ =>
        throw new IllegalArgumentException(s"Illegal file name '${src.getName}', should be formatted like 'en_US.po' (${src.getCanonicalPath})")
    }

    ret.result()
  }

  private def compileLocalesTask(projectSettings: ProjectSettings): Seq[File] = {
    val strategy = PoCompilerStrategy.getStrategy(projectSettings.compileLocalesStrategy)

    val doCompiling: GenerationContext => Unit = PoCompiler.doCompiling(strategy)
    val compileEnglishTags: GenerationContext => Unit = PoCompiler.compileEnglishTags(strategy)

    val r = withGenContext(
      projectSettings.compileLocalesSettings,
      "Language_%(l)_%(c).scala",
      "CompiledEnglishTags.scala")(
      perLang = doCompiling,
      englishTags = compileEnglishTags)

    if (strategy.generatesIndex) {
      val idx = {
        val langs = collectLangs(projectSettings.compileLocalesSettings)
        val pkg = projectSettings.compileLocalesSettings.localePackage

        val tgt = filePkg(projectSettings.compileLocalesSettings.target, pkg) / "Languages.scala"
        createParent(tgt)

        PoCompiler.generateIndex(pkg, tgt, langs, hasTags = false)

        tgt
      }

      r :+ idx
    } else
      r
  }

  private def withGenContext(task: TaskSettings, langFormat: String, tagFormat: String)
                            (perLang: GenerationContext => Unit, englishTags: GenerationContext => Unit): Seq[File] = {
    val baseTgt = task.target
    val pkg = task.localePackage
    val implicitCtx = None
    //      if ((includeImplicitContext in task).value) (implicitContext in task).value.filter(_.nonEmpty)
    //      else None
    val hasTags = task.taggetFile.isDefined

    val langPattern = "^([a-z]{2})_([A-Z]{2})\\.po$".r
    val ret = Seq.newBuilder[File]

    for (src <- task.sources) src.getName match {
      case langPattern(language, country) =>
        val tgt = filePkg(baseTgt, pkg) / StringUtils.interpolate(langFormat, "l" -> language, "c" -> country)
        createParent(tgt)

        val genCtx = GenerationContext(pkg, implicitCtx, LanguageId(language, country), hasTags, src, tgt, logger)
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

    for (t <- task.taggetFile) {
      val tgt = filePkg(baseTgt, pkg) / tagFormat

      val genCtx = GenerationContext(pkg, implicitCtx, LanguageId("xx", "XX"), hasTags, t, tgt, logger)
      englishTags(genCtx)
      ret += tgt
    }

    ret.result()
  }

  private def packageLocalesTask(projectSettings: ProjectSettings): Seq[File] = {
    val strategy = PoCompilerStrategy.getStrategy(projectSettings.compileLocalesStrategy)

    if (strategy.isPackagingNecessary)
      withGenContext(
        projectSettings.packageLocalesSettings,
        "data_%(l)_%(c).bin",
        "compiled_english_tags.bin")(
        perLang = PoCompiler.doPackaging,
        englishTags = PoCompiler.packageEnglishTags)
    else
      Seq.empty[File]
  }

  private def getFileTree(f: File): Stream[File] =
    f #:: (if (f.isDirectory) f.listFiles().toStream.flatMap(getFileTree)
    else Stream.empty)

  private def getSettings(project: Project): ProjectSettings = {
    val extension: ScalinguaSettingsExtension =
      project.getExtensions.create[ScalinguaSettingsExtension]("scalingua", classOf[ScalinguaSettingsExtension])

    val srcName = extension.getSrcPackage.getOrElse("main")
    val templateTarget = project.getBuildDir / "messages" / srcName + ".pot"
    val localePackage = extension.getLocalePackage.getOrElse("locales")
    val compileLocalesStrategy = extension.getCompileLocalesStrategy.getOrElse("ReadFromResources")
    //    val implicitContext = None
    //    val includeImplicitContext = true
    val taggedFile = Option(extension.getTaggetFile.getOrNull()).map(new File(_))

    val poFileExtension = ".po"
    val sourceDirectoryCompile = project.getProjectDir / "src" / srcName / localePackage

    val compileLocalesSources: Seq[File] = getFileTree(sourceDirectoryCompile).filter(p => p.getName.endsWith(poFileExtension))

    val targetCompile = project.getBuildDir / localePackage / srcName / "scala"
    val targetPackage = project.getBuildDir / localePackage / srcName / "resources"

    val compileLocalesSettings = CompileLocalesSettings(targetCompile, localePackage, compileLocalesSources, taggedFile)
    val packageLocalesSettings = PackageLocalesSettings(targetPackage, localePackage, compileLocalesSources, taggedFile)

    ProjectSettings(compileLocalesSettings, packageLocalesSettings, compileLocalesStrategy, templateTarget, srcName)
  }

  override def apply(project: Project): Unit = {

    val settings: ProjectSettings = getSettings(project)

    project.task("compileLocales").doLast { _: Task =>
      compileLocalesTask(settings)
    }

    project.task("packageLocales").doLast { _: Task =>
      packageLocalesTask(settings)
    }

    def appendScalaCompileOptions(options: String*): Unit = {
      project.getTasks.withType(classOf[ScalaCompile], (t: ScalaCompile) => {
        t.getScalaCompileOptions.setAdditionalParameters((
          Option(t.getScalaCompileOptions.getAdditionalParameters.asScala).getOrElse(Nil) ++
            options).asJava)
      })
    }

    appendScalaCompileOptions(
      s"-Xmacro-settings:scalingua:target=${project.getBuildDir.toString}/messages/${settings.srcName}.pot",
      s"-Xmacro-settings:scalingua:baseDir=${project.getProjectDir.toString}",
      "-Xmacro-settings:scalingua:escapeUnicode=true"
    )

    settings.compileLocalesSettings.taggetFile.foreach(tf =>
      appendScalaCompileOptions("-Xmacro-settings:scalingua:taggedFile=" + tf.getCanonicalPath))
  }
}
