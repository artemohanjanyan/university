package ru.ifmo.android_2015.worldcam;

import android.content.Context;
import android.database.sqlite.SQLiteDatabase;
import android.text.TextUtils;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;

import ru.ifmo.android_2015.db.CityDBHelper;
import ru.ifmo.android_2015.db.CityFileImporter_JsonReader;
import ru.ifmo.android_2015.util.DownloadUtils;
import ru.ifmo.android_2015.util.FileUtils;
import ru.ifmo.android_2015.util.ProgressCallback;

/**
 * Экран, выполняющий инициализацию базы данны городов. Данные читаются из ранее
 * скачанного файла (см. DownloadActivity) и записываются в базу данных.
 */
public class InitCityDBActivity extends ProgressTaskActivity {

    @Override
    protected ProgressTask createTask() {
        return new InitCityDBTask(this);
    }

    static class InitCityDBTask extends ProgressTask {

        InitCityDBTask(ProgressTaskActivity activity) {
            super(activity);
        }

        @Override
        protected void runTask() throws IOException {
            String fileName = new WorldcamPreferences(appContext).getCitiesFileName();
            if (TextUtils.isEmpty(fileName)) {
                throw new FileNotFoundException("File name is null");
            }
            File file = new File(appContext.getExternalFilesDir(null), fileName);
            importCitites(appContext, file, this);

            new WorldcamPreferences(appContext).saveDbIsReady(true);
        }
    }

    /**
     * Импортирует города из файла в базу данных.
     */
    static void importCitites(Context context,
                              File file,
                              ProgressCallback progressCallback) throws IOException {
        SQLiteDatabase db = CityDBHelper.getInstance(context).getWritableDatabase();
        new CityFileImporter_JsonReader(db).importCities(file, progressCallback);
    }
}
