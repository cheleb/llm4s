import com.typesafe.sbt.packager.docker.Cmd

// Define supported Scala versions
val scala213 = "2.13.14"
val scala3   = "3.3.6"

inThisBuild(
  List(
    crossScalaVersions := List(scala213, scala3),
    scalaVersion       := scala3,
    version            := "0.1.0-SNAPSHOT",
    organization       := "org.llm4s",
    organizationName   := "llm4s",
    versionScheme      := Some("early-semver"),
    // Scalafmt configuration
//    scalafmtOnCompile := true,
    // Maven central repository deployment
    homepage := Some(url("https://github.com/llm4s/")),
    ThisBuild / publishTo := {
      val centralSnapshots = "https://central.sonatype.com/repository/maven-snapshots/"
      if (isSnapshot.value) Some("central-snapshots".at(centralSnapshots))
      else localStaging.value
    },
    pgpPublicRing := file("/tmp/public.asc"),
    pgpSecretRing := file("/tmp/secret.asc"),
    pgpPassphrase := sys.env.get("PGP_PASSPHRASE").map(_.toArray),
    scmInfo := Some(
      ScmInfo(
        url("https://github.com/llm4s/llm4s/"),
        "scm:git:git@github.com:llm4s/llm4s.git"
      )
    ),
  )
)

// Scala options based on Scala version
def scalacOptionsForVersion(scalaVersion: String): Seq[String] =
  CrossVersion.partialVersion(scalaVersion) match {
    case Some((2, 13)) =>
      Seq(
        // "-Xfatal-warnings",   // Temporarily disabled for cross-compilation
        "-deprecation", // Emit warning and location for usages of deprecated APIs
        "-feature",     // Emit warning for feature usage
        "-unchecked"    // Enable warnings where generated code depends on assumptions
        // "-Wunused:imports"    // Temporarily disabled for cross-compilation
      )
    case Some((3, _)) =>
      Seq(
        "-explain",         // Explain errors in more detail
        "-Xfatal-warnings", // Fail on warnings
        "-source:3.3"       // Ensure Scala 3 syntax
      )
    case _ => Seq.empty
  }

// Common settings that apply to both Scala versions
lazy val commonSettings = Seq(
  Compile / scalacOptions := scalacOptionsForVersion(scalaVersion.value),

  // Source directories for cross-compilation
  Compile / unmanagedSourceDirectories ++= {
    val sourceDir = (Compile / sourceDirectory).value
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, 13)) => Seq(sourceDir / "scala-2.13")
      case Some((3, _))  => Seq(sourceDir / "scala-3")
      case _             => Nil
    }
  },
  Test / unmanagedSourceDirectories ++= {
    val sourceDir = (Test / sourceDirectory).value
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, 13)) => Seq(sourceDir / "scala-2.13")
      case Some((3, _))  => Seq(sourceDir / "scala-3")
      case _             => Nil
    }
  }
)

lazy val root = (project in file("."))
  .aggregate(shared, workspaceRunner)
  .dependsOn(shared)
  .settings(
    name := "llm4s",
    commonSettings,
    libraryDependencies ++= List(
      "com.azure"          % "azure-ai-openai" % "1.0.0-beta.16",
      "com.anthropic"      % "anthropic-java"  % "1.1.0",
      "ch.qos.logback"     % "logback-classic" % "1.5.18",
      "com.knuddels"       % "jtokkit"         % "1.1.0",
      "com.lihaoyi"       %% "upickle"         % "4.1.0",
      "com.lihaoyi"       %% "requests"        % "0.9.0",
      "org.java-websocket" % "Java-WebSocket"  % "1.5.3",
      "org.scalatest"     %% "scalatest"       % "3.2.19" % Test
    )
  )

lazy val shared = (project in file("shared"))
  .settings(
    name := "shared",
    commonSettings,
    libraryDependencies ++= List(
      "com.lihaoyi"   %% "upickle"         % "4.1.0",
      "ch.qos.logback" % "logback-classic" % "1.5.18",
      "org.scalatest" %% "scalatest"       % "3.2.19" % Test
    )
  )

lazy val workspaceRunner = (project in file("workspaceRunner"))
  .dependsOn(shared)
  .enablePlugins(JavaAppPackaging)
  .enablePlugins(DockerPlugin)
  .settings(
    Docker / maintainer := "llm4s",
    dockerExposedPorts  := Seq(8080),
    dockerBaseImage     := "eclipse-temurin:21-jdk",
    Compile / mainClass := Some("org.llm4s.runner.RunnerMain"),
    name                := "workspace-runner",
    commonSettings,
    libraryDependencies ++= List(
      "ch.qos.logback" % "logback-classic" % "1.5.18",
      "com.lihaoyi"   %% "upickle"         % "4.2.1",
      "com.lihaoyi"   %% "cask"            % "0.9.7",
      "com.lihaoyi"   %% "requests"        % "0.9.0",
      "org.scalatest" %% "scalatest"       % "3.2.19" % Test
    ),
    Docker / dockerBuildOptions := Seq("--platform=linux/amd64"),
    dockerCommands ++= Seq(
      Cmd("USER", "root"),
      Cmd("RUN", "apt-get update && apt-get install -y curl gnupg apt-transport-https ca-certificates zip unzip"),
      // Install SBT
      Cmd(
        "RUN",
        "echo 'deb https://repo.scala-sbt.org/scalasbt/debian all main' | tee /etc/apt/sources.list.d/sbt.list"
      ),
      Cmd(
        "RUN",
        "curl -sL 'https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x2EE0EA64E40A89B84B2DF73499E82A75642AC823' | apt-key add"
      ),
      Cmd("RUN", "apt-get update && apt-get install -y sbt"),
      // Install SDKMAN and use it to install Scala
      Cmd("RUN", "curl -s 'https://get.sdkman.io' | bash"),
      Cmd(
        "RUN",
        "bash -c 'source /root/.sdkman/bin/sdkman-init.sh && sdk install scala " + scala3 + " && sdk install scala " + scala213 + "'"
      ),
      Cmd("ENV", "PATH=/root/.sdkman/candidates/scala/current/bin:$PATH")
    )
  )
  .settings(
    publish / skip := true
  )

lazy val samples = (project in file("samples"))
  .dependsOn(shared, root)
  .settings(
    name := "samples",
    commonSettings,
    libraryDependencies ++= List(
      "ch.qos.logback" % "logback-classic" % "1.5.18",
      "org.scalatest" %% "scalatest"       % "3.2.19" % Test
    )
  )
  .settings(
    publish / skip := true
  )

val crossLibDependencies = Seq(
  "org.llm4s"     %% "llm4s"     % "0.1.0-SNAPSHOT",
  "org.scalatest" %% "scalatest" % "3.2.19" % Test
)

lazy val crossTestScala2 = (project in file("crosstest/scala2"))
  .settings(
    name               := "crosstest-scala2",
    scalaVersion       := scala213,
    crossScalaVersions := Seq(scala213),
    resolvers += Resolver.mavenLocal,
    resolvers += Resolver.defaultLocal,
    libraryDependencies ++= crossLibDependencies
  )

lazy val crossTestScala3 = (project in file("crosstest/scala3"))
  .settings(
    name               := "crosstest-scala3",
    scalaVersion       := scala3,
    crossScalaVersions := Seq(scala3),
    resolvers += Resolver.mavenLocal,
    resolvers += Resolver.defaultLocal,
    scalacOptions ++= Seq(
      "-language:strictEquality",
      "-Ysafe-init",
      "-deprecation",
      "-Wunused:imports",
      "-Xfatal-warnings"
    ),
    libraryDependencies ++= crossLibDependencies
  )

// Commands remain the same
addCommandAlias("buildAll", ";clean;+compile;+test")
addCommandAlias("publishAll", ";clean;+publish")
addCommandAlias("testAll", ";+test")
addCommandAlias("compileAll", ";+compile")
addCommandAlias("testCross", ";crossTestScala2/test;crossTestScala3/test")
// Add this to your build.sbt
addCommandAlias("fullCrossTest", ";clean ;crossTestScala2/clean ;crossTestScala3/clean ;+publishLocal ;testCross")

// MiMa settings remain the same
mimaPreviousArtifacts := Set(
  organization.value %% "llm4s" % "0.1.0-SNAPSHOT"
)
