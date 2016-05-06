package ru.ifmo.android_2015.worldcam;

import android.app.Activity;
import android.os.Bundle;
import android.util.Log;
import android.widget.ProgressBar;
import android.widget.TextView;

import ru.ifmo.android_2015.lesson6.worldcam.R;

/**
 * Created by dmitry.trunin on 14.11.2015.
 */
public abstract class ProgressTaskActivity extends Activity {

    // Индикатор прогресса
    private ProgressBar progressBarView;
    // Заголовок
    private TextView titleTextView;
    // Выполняющийся таск загрузки файла
    private ProgressTask downloadTask;

    protected abstract ProgressTask createTask();

    @Override
    @SuppressWarnings("deprecation")
    protected void onCreate(Bundle savedInstanceState) {
        Log.d(TAG, "onCreate");
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_init_splash);

        titleTextView = (TextView) findViewById(R.id.title_text);
        progressBarView = (ProgressBar) findViewById(R.id.progress_bar);

        progressBarView.setMax(100);

        if (savedInstanceState != null) {
            // Пытаемся получить ранее запущенный таск
            downloadTask = (ProgressTask) getLastNonConfigurationInstance();
        }
        if (downloadTask == null) {
            // Создаем новый таск, только если не было ранее запущенного таска
            downloadTask = createTask();
            downloadTask.execute();
        } else {
            // Передаем в ранее запущенный таск текущий объект Activity
            downloadTask.attachActivity(this);
        }
    }

    @Override
    @SuppressWarnings("deprecation")
    public Object onRetainNonConfigurationInstance() {
        // Этот метод вызывается при смене конфигурации, когда текущий объект
        // Activity уничтожается. Объект, который мы вернем, не будет уничтожен,
        // и его можно будет использовать в новом объекте Activity
        return downloadTask;
    }

    /**
     * Вызываем на UI потоке для обновления отображения прогресса и
     * состояния в текущей активности.
     */
    void updateView(ProgressTask task) {
        titleTextView.setText(getTextForState(task));
        progressBarView.setProgress(task.getProgress());
    }

    String getTextForState(ProgressTask task) {
        final TaskState state = task.getState();
        switch (state) {
            case NEW:       return getString(R.string.new_task);
            case RUNNING:   return getString(R.string.running);
            default:
            case ERROR:     return getString(R.string.error);
            case DONE:
            {
                final double timeSec = task.getExecutionTimeMs() / 1000.0;
                return getString(R.string.done_sec_format, timeSec);
            }
        }
    }

    protected final String TAG = getClass().getSimpleName();

}
