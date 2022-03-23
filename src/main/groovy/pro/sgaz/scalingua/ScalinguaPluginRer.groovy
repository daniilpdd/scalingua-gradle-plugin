package pro.sgaz.scalingua

import org.gradle.api.DefaultTask
import org.gradle.api.Plugin
import org.gradle.api.Project
import org.gradle.api.provider.Property
import org.gradle.api.tasks.TaskAction

interface Ext {
    Property<String> getMessage()
}

class Kek extends DefaultTask {
    @TaskAction
    public static void PrintStrategy(String msg) {
//        def kek = PoCompilerStrategy.getStrategy("ReadFromResources").toString()
//        println kek
    }
}

class ScalinguaPluginRer implements Plugin<Project> {
    @Override
    void apply(Project project) {
        def ext = project.extensions.create('keks', Ext)
        project.task('kek1') {
            doLast {
                Kek.PrintStrategy(ext.message.get())
                println 'strategy ' + ext.message.get()
            }
        }

        project.task('kek')
    }
}
