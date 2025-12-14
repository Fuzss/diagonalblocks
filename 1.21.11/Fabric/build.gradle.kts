plugins {
    id("fuzs.multiloader.multiloader-convention-plugins-fabric")
}

dependencies {
    modApi(libs.fabricapi.fabric)
    modApi(libs.puzzleslib.fabric)
}

multiloader {
    modFile {
        packagePrefix.set("impl")
        library.set(true)
    }

    mixins {
        clientMixin("ClientLevelFabricMixin")
    }
}
