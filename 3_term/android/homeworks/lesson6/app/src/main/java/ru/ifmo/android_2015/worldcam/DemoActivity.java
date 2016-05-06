package ru.ifmo.android_2015.worldcam;

import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.os.AsyncTask;
import android.os.Bundle;
import android.text.TextUtils;
import android.util.Log;
import android.view.View;

import ru.ifmo.android_2015.lesson6.worldcam.R;

/**
 * Created by dmitry.trunin on 15.11.2015.
 */
public class DemoActivity extends Activity {

    LoadPrefsTask loadPrefsTask;
    WorldcamPreferences prefs;

    @Override
    @SuppressWarnings("deprecation")
    protected void onCreate(Bundle savedInstanceState) {
        Log.d(TAG, "onCreate");
        super.onCreate(savedInstanceState);
        setContentView(R.layout.demo_activity);

        findViewById(R.id.btn_download).setEnabled(false);
        findViewById(R.id.btn_init_db).setEnabled(false);
        findViewById(R.id.btn_json_object).setEnabled(false);
        findViewById(R.id.btn_json_reader).setEnabled(false);
        findViewById(R.id.btn_clean).setEnabled(false);
        findViewById(R.id.btn_city_list).setEnabled(false);

        if (savedInstanceState != null) {
            loadPrefsTask = (LoadPrefsTask) getLastNonConfigurationInstance();
        }
        if (loadPrefsTask != null) {
            loadPrefsTask.attachActivity(this);
            WorldcamPreferences prefs = loadPrefsTask.getPrefernces();
            if (prefs != null) {
                onPrefsLoaded(prefs);
            }

        } else {
            loadPrefsTask = new LoadPrefsTask(this);
            loadPrefsTask.execute();
        }
    }

    @Override
    protected void onResume() {
        super.onResume();
        updateButtonsByPrefs();
    }

    protected void onPrefsLoaded(WorldcamPreferences prefs) {
        Log.d(TAG, "onPrefsLoaded");
        this.prefs = prefs;
        updateButtonsByPrefs();
    }

    protected void updateButtonsByPrefs() {
        findViewById(R.id.btn_download).setEnabled(prefs != null);
        findViewById(R.id.btn_clean).setEnabled(true);

        if (prefs != null) {
            String citiesFileName = prefs.getCitiesFileName();
            boolean fileIsDownloaded = !TextUtils.isEmpty(citiesFileName);
            findViewById(R.id.btn_init_db).setEnabled(fileIsDownloaded);
            findViewById(R.id.btn_json_object).setEnabled(fileIsDownloaded);
            findViewById(R.id.btn_json_reader).setEnabled(fileIsDownloaded);
            findViewById(R.id.btn_city_list).setEnabled(prefs.getDbIsReady());
        }
    }

    public void onButtonClick(View button) {
        Log.d(TAG, "onButtonClick: " + getResources().getResourceEntryName(button.getId()));
        switch (button.getId()) {
            case R.id.btn_download:
                startActivity(new Intent(this, DownloadActivity.class));
                break;

            case R.id.btn_init_db:
                startActivity(new Intent(this, InitCityDBActivity.class));
                break;

            case R.id.btn_clean:
                startActivity(new Intent(this, CleanActivity.class));
                break;

            case R.id.btn_json_reader:
            {
                Intent intent = new Intent(this, JsonParserActivity.class);
                intent.putExtra(JsonParserActivity.EXTRA_PARSER_TYPE,
                        JsonParserActivity.PARSER_JSON_READER);
                startActivity(intent);
                break;
            }

            case R.id.btn_json_object:
            {
                Intent intent = new Intent(this, JsonParserActivity.class);
                intent.putExtra(JsonParserActivity.EXTRA_PARSER_TYPE,
                        JsonParserActivity.PARSER_JSON_OBJECT);
                startActivity(intent);
                break;
            }

            case R.id.btn_city_list:
                startActivity(new Intent(this, SelectCityActivity.class));
                break;
        }
    }

    @Override
    @SuppressWarnings("deprecation")
    public Object onRetainNonConfigurationInstance() {
        return loadPrefsTask;
    }

    static class LoadPrefsTask extends AsyncTask<Void, Void, WorldcamPreferences> {

        private final Context context;
        private DemoActivity activity;
        private WorldcamPreferences prefs;

        public LoadPrefsTask(DemoActivity activity) {
            this.activity = activity;
            this.context = activity.getApplicationContext();
        }

        void attachActivity(DemoActivity activity) {
            this.activity = activity;
        }

        WorldcamPreferences getPrefernces() {
            return prefs;
        }

        @Override
        protected WorldcamPreferences doInBackground(Void... params) {
            Log.d(TAG, "start loading preferences...");
            WorldcamPreferences prefs = new WorldcamPreferences(context);
            prefs.awaitLoaded();
            Log.d(TAG, "loading preferences finished");
            return prefs;
        }

        @Override
        protected void onPostExecute(WorldcamPreferences prefs) {
            this.prefs = prefs;
            if (activity != null) {
                activity.onPrefsLoaded(prefs);
            }
        }
    }

    private static final String TAG = "DemoActivity";
}
