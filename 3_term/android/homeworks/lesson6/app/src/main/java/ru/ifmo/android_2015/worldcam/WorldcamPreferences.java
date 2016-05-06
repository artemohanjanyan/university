package ru.ifmo.android_2015.worldcam;

import android.content.Context;
import android.content.SharedPreferences;
import android.util.Log;

/**
 * Created by dmitry.trunin on 15.11.2015.
 */
public class WorldcamPreferences {

    private static final String PREFS_NAME = "worldcam";

    private static final String KEY_CITIES_FILE = "cities_file";
    private static final String KEY_DB_IS_READY = "db_is_ready";

    private final Context context;
    private final SharedPreferences prefs;

    public WorldcamPreferences(Context context) {
        this.context = context.getApplicationContext();
        prefs = context.getSharedPreferences(PREFS_NAME, Context.MODE_PRIVATE);
    }

    public void awaitLoaded() {
        prefs.getAll();
    }

    public String getCitiesFileName() {
        return prefs.getString(KEY_CITIES_FILE, null);
    }

    public void saveCitiesFileName(String fileName) {
        prefs.edit().putString(KEY_CITIES_FILE, fileName).apply();
    }

    public boolean getDbIsReady() {
        return prefs.getBoolean(KEY_DB_IS_READY, false);
    }

    public void saveDbIsReady(boolean dbIsReady) {
        prefs.edit().putBoolean(KEY_DB_IS_READY, dbIsReady).apply();
    }

    public void clear() {
        prefs.edit().clear().commit();
        Log.d(TAG, "Cleared worldcam preferences");
    }

    private static final String TAG = "WorldcamPrefs";
}
