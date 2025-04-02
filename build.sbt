inThisBuild(
  List(
    scalaVersion     := "2.13.16",
    version          := "0.1.0-SNAPSHOT",
    organization     := "org.llm4s",
    organizationName := "llm4s",

    // Scalafmt configuration
    scalafmtOnCompile := true,
    // Maven central repository deployment
    homepage               := Some(url("https://github.com/llm4s/")),
    sonatypeCredentialHost := "s01.oss.sonatype.org",
    sonatypeRepository     := "https://s01.oss.sonatype.org/service/local",
    pgpPublicRing          := file("/tmp/public.asc"),
    pgpSecretRing          := file("/tmp/secret.asc"),
    pgpPassphrase          := sys.env.get("PGP_PASSWORD").map(_.toArray),
    scmInfo := Some(
      ScmInfo(
        url("https://github.com/llm4s/llm4s/"),
        "scm:git:git@github.com:llm4s/llm4s.git"
      )
    ),
  )
)

lazy val root = (project in file("."))
  .aggregate(shared, workspaceRunner)
  .dependsOn(shared)
  .dependsOn(shared)
  .settings(
    name := "llm4s",
    libraryDependencies ++= List(
      "com.azure"      % "azure-ai-openai" % "1.0.0-beta.15",
      "com.anthropic"  % "anthropic-java"  % "0.9.1",
      "ch.qos.logback" % "logback-classic" % "1.5.18",
      "com.knuddels" % "jtokkit" % "1.1.0",
      "com.lihaoyi"   %% "upickle"         % "4.1.0",
      "com.lihaoyi"   %% "requests"        % "0.9.0",
      "org.scalatest" %% "scalatest"       % "3.2.19" % Test
    )
  )

lazy val shared = (project in file("shared"))
  .settings(
    name := "shared",
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
//    Compile / mainClass := Some("com.llm4s.runner.RunnerMain"),
    name := "workspace-runner",
    libraryDependencies ++= List(
      "ch.qos.logback" % "logback-classic" % "1.5.18",
      "com.lihaoyi"   %% "upickle"         % "4.1.0",
      "com.lihaoyi"   %% "cask"            % "0.10.2",
      "org.scalatest" %% "scalatest"       % "3.2.19" % Test
    )
  )
  .settings(
    publish / skip := true
  )

lazy val samples = (project in file("samples"))
  .dependsOn(shared, root)
  .settings(
    name := "samples",
    libraryDependencies ++= List(
      "ch.qos.logback" % "logback-classic" % "1.5.18",
      "org.scalatest" %% "scalatest"       % "3.2.19" % Test
    )
  )
  .settings(
    publish / skip := true
  )
