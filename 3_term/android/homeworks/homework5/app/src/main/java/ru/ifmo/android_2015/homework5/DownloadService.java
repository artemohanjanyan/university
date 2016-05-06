package ru.ifmo.android_2015.homework5;

import android.app.IntentService;
import android.content.Intent;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.support.v4.content.LocalBroadcastManager;
import android.util.JsonReader;
import android.util.Log;

import java.io.InputStreamReader;
import java.net.URL;

import ru.ifmo.android_2015.homework5.model.City;
import ru.ifmo.android_2015.homework5.webcams.Webcams;

public class DownloadService extends IntentService {
    private static final String NAME = "download-service";

    public static final String INFO = "ru.ifmo.android_2015.homework5.INFO";
    public static final String IMAGE = "ru.ifmo.android_2015.homework5.IMAGE";
    public static final String ERROR = "ru.ifmo.android_2015.homework5.ERROR";
    public static final String DATA = "ru.ifmo.android_2015.homework5.DATA";

    public DownloadService() {
        super(NAME);
    }

    @Override
    protected void onHandleIntent(Intent intent) {
        City city = intent.getExtras().getParcelable(City.NAME);
        String webcamName = null;
        URL previewUrl = null;
        Bitmap bitmap = null;
        JsonReader reader = null;

        // No JSONObjects were harmed in the writing of this code

        try {
            // Download camera info
            Log.d(CityCamActivity.TAG, "Downloading JSON...");

            URL jsonUrl = Webcams.createNearbyUrl(city.latitude, city.longitude);
            reader = new JsonReader(new InputStreamReader(jsonUrl.openStream()));

            Log.d(CityCamActivity.TAG, "Reading JSON...");
            reader.beginObject();
            while (reader.hasNext() && !reader.nextName().equals("webcams")) {
                reader.skipValue();
            }
            reader.beginObject();
            Log.d(CityCamActivity.TAG, "Webcams found...");
            while (reader.hasNext() && !reader.nextName().equals("webcam")) {
                reader.skipValue();
            }
            reader.beginArray();
            reader.beginObject();
            Log.d(CityCamActivity.TAG, "Webcam found...");
            while (reader.hasNext() && (webcamName == null || previewUrl == null)) {
                String nextName = reader.nextName();
                switch (nextName) {
                    case "title":
                        webcamName = reader.nextString();
                        break;
                    case "preview_url":
                        previewUrl = new URL(reader.nextString());
                        break;
                    default:
                        reader.skipValue();
                        break;
                }
            }
            if (webcamName == null || previewUrl == null) {
                throw new Exception();
            }

            Log.d(CityCamActivity.TAG, "JSON read");
            Log.d(CityCamActivity.TAG, "Camera name: " + webcamName);
            Log.d(CityCamActivity.TAG, "Preview URL: " + previewUrl.toString());

            // Show camera name first
            Intent infoIntent = new Intent(INFO);
            infoIntent.putExtra(DATA, webcamName);
            LocalBroadcastManager.getInstance(this).sendBroadcast(infoIntent);

            // Download picture
            bitmap = BitmapFactory.decodeStream(previewUrl.openStream());
            if (bitmap == null) {
                throw new Exception();
            }
            Intent imageIntent = new Intent(IMAGE);
            imageIntent.putExtra(DATA, bitmap);
            LocalBroadcastManager.getInstance(this).sendBroadcast(imageIntent);
        } catch (Exception ignored) {
            Log.d(CityCamActivity.TAG, "Download unsuccessful");
            Intent errorIntent = new Intent(ERROR);
            LocalBroadcastManager.getInstance(this).sendBroadcast(errorIntent);
        } finally {
            if (reader != null) {
                try {
                    reader.close();
                } catch (Exception ignored) {
                }
            }
        }
    }
}
