package ru.ifmo.android_2015.homework5;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.graphics.Bitmap;
import android.os.Bundle;
import android.support.v4.content.LocalBroadcastManager;
import android.support.v7.app.ActionBar;
import android.support.v7.app.AppCompatActivity;
import android.util.Log;
import android.view.View;
import android.widget.ImageView;
import android.widget.ProgressBar;
import android.widget.TextView;

import ru.ifmo.android_2015.homework5.model.City;

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

    BroadcastReceiver receiver;
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
            startService(new Intent(this, DownloadService.class).putExtra(City.NAME, city));
        }

        // Здесь должен быть код, инициирующий асинхронную загрузку изображения с веб-камеры
        // в выбранном городе.

        receiver = new BroadcastReceiver() {
            @Override
            public void onReceive(Context context, Intent intent) {
                switch (intent.getAction()) {
                    case DownloadService.INFO:
                        camNameView.setText(intent.getStringExtra(DownloadService.DATA));
                        break;

                    case DownloadService.IMAGE:
                        progressView.setVisibility(View.INVISIBLE);
                        camImageView.setVisibility(View.VISIBLE);
                        bitmap = intent.getParcelableExtra(DownloadService.DATA);
                        camImageView.setImageBitmap(bitmap);
                        break;

                    case DownloadService.ERROR:
                        camNameView.setText(R.string.download_error);
                        break;
                }
            }
        };

        IntentFilter intentFilter = new IntentFilter();
        intentFilter.addAction(DownloadService.INFO);
        intentFilter.addAction(DownloadService.IMAGE);
        intentFilter.addAction(DownloadService.ERROR);

        LocalBroadcastManager.getInstance(this).registerReceiver(receiver, intentFilter);
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
            startService(new Intent(this, DownloadService.class).putExtra(City.NAME, city));
        }

        camNameView.setText(savedInstanceState.getCharSequence(NAME_STRING));
    }

    @Override
    protected void onDestroy() {
        LocalBroadcastManager.getInstance(this).unregisterReceiver(receiver);
        super.onDestroy();
    }

    static final String TAG = "CityCam";
}
