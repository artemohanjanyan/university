package ru.ifmo.android_2015.db.util;

import android.content.Context;
import android.database.DatabaseErrorHandler;
import android.database.sqlite.SQLiteDatabase;
import android.database.sqlite.SQLiteException;
import android.util.Log;

import java.io.File;

public class DatabaseCorruptionHandler implements DatabaseErrorHandler {

    private final Context context;
    private final String dbName;

    public DatabaseCorruptionHandler(Context context, String dbName) {
        this.context = context.getApplicationContext();
        this.dbName = dbName;
    }

    @Override
    public void onCorruption(SQLiteDatabase db) {
        final boolean databaseOk = db.isDatabaseIntegrityOk();
        // close the database
        try {
            db.close();
        } catch (SQLiteException e) {
            /* ignore */
        }
        final File dbFile = context.getDatabasePath(dbName);
        if (databaseOk) {
            // database is just fine. no need to delete the database file
            Log.e(LOG_TAG, "no corruption in the database: " + dbFile.getPath());
        } else {
            // database is corrupt. delete the database file
            Log.e(LOG_TAG, "deleting the database file: " + dbFile.getPath());
            dbFile.delete();
        }
    }

    private static final String LOG_TAG = "DBCorruption";
}
