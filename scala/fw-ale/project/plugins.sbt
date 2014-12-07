
lazy val root = file("../api-framework-plugin")

lazy val plugins = project in file(".") dependsOn root

