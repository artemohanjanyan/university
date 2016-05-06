package ru.ifmo.android_2015.citycam;

import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.os.AsyncTask;
import android.util.JsonReader;
import android.util.Log;
import android.view.View;

import java.io.InputStreamReader;
import java.net.URL;

import ru.ifmo.android_2015.citycam.model.City;
import ru.ifmo.android_2015.citycam.webcams.Webcams;

/**
 * Created by Artem Ohanjanyan
 */
public class DownloadTask extends AsyncTask<Void, String, Bitmap> {
    CityCamActivity activity;

    public DownloadTask(CityCamActivity activity) {
        this.activity = activity;
    }

    public void attachActivity(CityCamActivity activity) {
        this.activity = activity;
    }

    @Override
    protected Bitmap doInBackground(Void... params) {
        try {
            return unsafeDoInBackground();
        } catch (Exception e) {
            return null;
        }
    }

    /**
     * @return Image {@code Bitmap} in case of successful download, {@code null} otherwise
     * @throws Exception basically any exception which can be thrown
     * during downloading or parsing JSON
     */
    private Bitmap unsafeDoInBackground() throws Exception {
        City city = activity.city;
        String webcamName = null;
        URL previewUrl = null;

        // No JSONObjects were harmed in the writing of this code

        // Download camera info
        Log.d(CityCamActivity.TAG, "Downloading JSON...");

        URL jsonUrl = Webcams.createNearbyUrl(city.latitude, city.longitude);
        JsonReader reader = new JsonReader(new InputStreamReader(jsonUrl.openStream()));

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
            return null;
        }

        Log.d(CityCamActivity.TAG, "JSON read");
        Log.d(CityCamActivity.TAG, "Camera name: " + webcamName);
        Log.d(CityCamActivity.TAG, "Preview URL: " + previewUrl.toString());

        // Show camera name first
        publishProgress(webcamName);

        // Download picture
        return BitmapFactory.decodeStream(previewUrl.openStream());
    }

    /**
     * Show camera name
     * @param values camera name
     */
    @Override
    protected void onProgressUpdate(String... values) {
        super.onProgressUpdate(values);
        Log.d(CityCamActivity.TAG, "Showing camera name...");
        activity.camNameView.setText(values[0]);
    }

    @Override
    protected void onPostExecute(Bitmap bitmap) {
        super.onPostExecute(bitmap);

        Log.d(CityCamActivity.TAG, "onPostExecute");

        if (bitmap == null) {
            Log.d(CityCamActivity.TAG, "Download unsuccessful");
            activity.camNameView.setText(R.string.download_error);
        } else {
            Log.d(CityCamActivity.TAG, "Download successful");
            activity.progressView.setVisibility(View.INVISIBLE);
            activity.camImageView.setVisibility(View.VISIBLE);
            activity.camImageView.setImageBitmap(bitmap);
            activity.bitmap = bitmap;
        }
    }
}
