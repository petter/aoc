import org.jetbrains.kotlin.gradle.tasks.KotlinCompile

plugins {
    kotlin("jvm") version "1.5.31"
    application
}

group = "me.petter"
version = "1.0-SNAPSHOT"

repositories {
    mavenCentral()
}


tasks {
    day1
}

tasks.withType<KotlinCompile>() {
    kotlinOptions.jvmTarget = "1.8"
}