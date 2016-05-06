package ru.ifmo.android_2015.worldcam;

import android.app.Application;

import com.facebook.stetho.Stetho;

/**
 * Класс Application, прописывается в AndroidManifest.xml
 */
public class DebugApplication extends Application {

    @Override
    public void onCreate() {
        super.onCreate();
        // Инициализируем библиотеку для отладки Stetho
        Stetho.initializeWithDefaults(this);
    }
}
