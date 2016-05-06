package ru.ifmo.android_2015.db;

import android.content.Context;
import android.database.sqlite.SQLiteDatabase;
import android.database.sqlite.SQLiteOpenHelper;
import android.util.Log;

import java.io.File;

import ru.ifmo.android_2015.db.util.DatabaseCorruptionHandler;

/**
 * Класс для доступа к БД городов.
 */
public class CityDBHelper extends SQLiteOpenHelper {

    private static final String DB_FILE_NAME = "cities.db";

    private static final int DB_VERSION_1 = 1;

    private static volatile CityDBHelper instance;

    public static CityDBHelper getInstance(Context context) {
        if (instance == null) {
            synchronized (CityDBHelper.class) {
                if (instance == null) {
                    instance = new CityDBHelper(context);
                }
            }
        }
        return instance;
    }

    private final Context context;

    public CityDBHelper(Context context) {
        super(context, DB_FILE_NAME, null /*factory*/, DB_VERSION_1,
                new DatabaseCorruptionHandler(context, DB_FILE_NAME));
        this.context = context.getApplicationContext();
    }

    @Override
    public void onCreate(SQLiteDatabase db) {
        Log.d(LOG_TAG, "onCreate: " + CityContract.Cities.CREATE_TABLE);
        db.execSQL(CityContract.Cities.CREATE_TABLE);
    }

    @Override
    public void onUpgrade(SQLiteDatabase db, int oldVersion, int newVersion) {
        Log.d(LOG_TAG, "onUpgrade: oldVersion=" + oldVersion + " newVersion=" + newVersion);
    }

    public void dropDb() {
        SQLiteDatabase db = getWritableDatabase();
        if (db.isOpen()) {
            try {
                db.close();
            } catch (Exception e) {
                Log.w(LOG_TAG, "Failed to close DB");
            }
        }
        final File dbFile = context.getDatabasePath(DB_FILE_NAME);
        try {
            Log.d(LOG_TAG, "deleting the database file: " + dbFile.getPath());
            if (!dbFile.delete()) {
                Log.w(LOG_TAG, "Failed to delete database file: " + dbFile);
            }
            Log.d(LOG_TAG, "Deleted DB file: " + dbFile);
        } catch (Exception e) {
            Log.w(LOG_TAG, "Failed to delete database file: " + dbFile, e);
        }
    }

    private static final String LOG_TAG = "CitiesDB";
}
