package ru.ifmo.android_2015.citycam;

import android.graphics.Bitmap;
import android.os.Bundle;
import android.support.v7.app.ActionBar;
import android.support.v7.app.AppCompatActivity;
import android.util.Log;
import android.view.View;
import android.widget.ImageView;
import android.widget.ProgressBar;
import android.widget.TextView;

import ru.ifmo.android_2015.citycam.model.City;

/**
 * Экран, показывающий веб-камеру одного выбранного города.
 * Выбранный город передается в extra параметрах.
 */
public class CityCamActivity extends AppCompatActivity {

    /**
     * Обязательный extra параметр - объект City, камеру которого надо показать.
     */
    public static final String EXTRA_CITY = "city";

    City city;

    ImageView camImageView;
    ProgressBar progressView;
    TextView camNameView;

    DownloadTask downloadTask;
    Bitmap bitmap;

    static final String IMAGE_BITMAP = "imageBitmap";
    static final String NAME_STRING = "nameString";


    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        city = getIntent().getParcelableExtra(EXTRA_CITY);
        if (city == null) {
            Log.w(TAG, "City object not provided in extra parameter: " + EXTRA_CITY);
            finish();
        }

        setContentView(R.layout.activity_city_cam);
        camImageView = (ImageView) findViewById(R.id.cam_image);
        progressView = (ProgressBar) findViewById(R.id.progress);
        camNameView = (TextView) findViewById(R.id.cam_name);

        ActionBar actionBar = getSupportActionBar();
        if (actionBar != null) {
            actionBar.setTitle(city.name);
        }

        if (savedInstanceState == null) {
            camImageView.setVisibility(View.INVISIBLE);
            progressView.setVisibility(View.VISIBLE);
        }

        // Здесь должен быть код, инициирующий асинхронную загрузку изображения с веб-камеры
        // в выбранном городе.
        if (savedInstanceState != null && getLastCustomNonConfigurationInstance() != null) {
            downloadTask = (DownloadTask) getLastCustomNonConfigurationInstance();
            downloadTask.attachActivity(this);
        } else {
            Log.d(TAG, "task created");
            downloadTask = new DownloadTask(this);
            downloadTask.execute();
        }
    }

    @Override
    protected void onSaveInstanceState(Bundle outState) {
        outState.putParcelable(IMAGE_BITMAP, bitmap);
        outState.putCharSequence(NAME_STRING, camNameView.getText());

        super.onSaveInstanceState(outState);
    }

    @Override
    protected void onRestoreInstanceState(Bundle savedInstanceState) {
        super.onRestoreInstanceState(savedInstanceState);

        bitmap = savedInstanceState.getParcelable(IMAGE_BITMAP);
        if (bitmap != null) {
            camImageView.setImageBitmap(bitmap);
            camImageView.setVisibility(View.VISIBLE);
            progressView.setVisibility(View.INVISIBLE);
        } else {
            camImageView.setVisibility(View.INVISIBLE);
            progressView.setVisibility(View.VISIBLE);
        }

        camNameView.setText(savedInstanceState.getCharSequence(NAME_STRING));
    }

    @Override
    public Object onRetainCustomNonConfigurationInstance() {
        return downloadTask;
    }

    static final String TAG = "CityCam";
}
