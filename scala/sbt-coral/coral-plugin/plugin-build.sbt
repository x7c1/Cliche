sbtPlugin := true

val plugin = project in file(".") dependsOn file("../coral-lib")
