package ru.ifmo.android_2015.worldcam;

import java.io.IOException;

import ru.ifmo.android_2015.db.CityDBHelper;
import ru.ifmo.android_2015.util.FileUtils;

/**
 * Экран, выполняющий очистку: удаляет все файлы, сохраненные настройки и БД
 */
public class CleanActivity extends ProgressTaskActivity {

    @Override
    protected ProgressTask createTask() {
        return new CleanTask(this);
    }

    static class CleanTask extends ProgressTask {

        CleanTask(ProgressTaskActivity activity) {
            super(activity);
        }

        @Override
        protected void runTask() throws IOException {
            new WorldcamPreferences(appContext).clear();
            FileUtils.cleanExternalFilesDir(appContext);
            CityDBHelper.getInstance(appContext).dropDb();
        }
    }
}
