package ru.ifmo.android_2015.worldcam;

import android.content.Context;
import android.os.AsyncTask;
import android.os.SystemClock;
import android.util.Log;

import java.io.File;

import ru.ifmo.android_2015.util.ProgressCallback;

/**
 * Таск, выполняющий скачивание файла в фоновом потоке.
 */
public abstract class ProgressTask extends AsyncTask<Void, Integer, TaskState>
        implements ProgressCallback {

    // Context приложения (Не Activity!) для доступа к файлам
    protected final Context appContext;
    // Текущий объект Activity, храним для обновления отображения
    private ProgressTaskActivity activity;

    // Текущее состояние загрузки
    private volatile TaskState state = TaskState.NEW;
    // Прогресс загрузки от 0 до 100
    private int progress;

    // Момент времени, когда началось выполнение задачи
    private long executionStartTs;

    // Время, потраченное на выполнение задачи, либо -1, если импорт еще не завершен
    private long executionTimeMs = -1;

    ProgressTask(ProgressTaskActivity activity) {
        this.appContext = activity.getApplicationContext();
        this.activity = activity;
    }

    protected abstract void runTask() throws Exception;

    TaskState getState() {
        return state;
    }

    int getProgress() {
        return progress;
    }

    long getExecutionTimeMs() {
        return executionTimeMs;
    }

    /**
     * Этот метод вызывается, когда новый объект Activity подключается к
     * данному таску после смены конфигурации.
     *
     * @param activity новый объект Activity
     */
    void attachActivity(ProgressTaskActivity activity) {
        this.activity = activity;
        activity.updateView(this);
    }


    /**
     * Вызывается в UI потоке из execute() до начала выполнения таска.
     */
    @Override
    protected void onPreExecute() {
        if (activity != null) {
            activity.updateView(this);
        }
    }

    /**
     * Скачивание файла в фоновом потоке. Возвращает результат:
     *      0 -- если файл успешно скачался
     *      1 -- если произошла ошибка
     */
    @Override
    protected final TaskState doInBackground(Void... ignore) {
        progress = 0;
        state = TaskState.RUNNING;
        publishProgress(0);

        executionStartTs = System.currentTimeMillis();
        try {
            runTask();

            executionTimeMs = System.currentTimeMillis() - executionStartTs;
            state = TaskState.DONE;

        } catch (Exception e) {
            Log.e(LOG_TAG, "Error downloading file: " + e, e);
            state = TaskState.ERROR;
        }
        return state;
    }

    // Метод ProgressCallback, вызывается в фоновом потоке из downloadFile
    @Override
    public void onProgressChanged(int progress) {
        publishProgress(progress);
    }

    // Метод AsyncTask, вызывается в UI потоке в результате вызова publishProgress
    @Override
    protected void onProgressUpdate(Integer... values) {
        if (values.length > 0) {
            int progress = values[values.length - 1];
            this.progress = progress;
            if (activity != null) {
                activity.updateView(this);
            }
        }
    }

    @Override
    protected void onPostExecute(TaskState state) {
        // Проверяем код, который вернул doInBackground и показываем текст в зависимости
        // от результата
        this.state = state;
        if (state == TaskState.DONE) {
            progress = 100;
        }
        if (activity != null) {
            activity.updateView(this);
        }
    }

    protected final String LOG_TAG = getClass().getSimpleName();
}
