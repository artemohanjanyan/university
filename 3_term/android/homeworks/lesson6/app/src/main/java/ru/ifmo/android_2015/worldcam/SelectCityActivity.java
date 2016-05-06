package ru.ifmo.android_2015.worldcam;

import android.app.Activity;
import android.database.Cursor;
import android.database.sqlite.SQLiteDatabase;
import android.os.Bundle;
import android.text.TextUtils;
import android.util.Log;
import android.view.View;
import android.widget.AdapterView;
import android.widget.AutoCompleteTextView;
import android.widget.FilterQueryProvider;
import android.widget.SimpleCursorAdapter;

import ru.ifmo.android_2015.db.CityContract;
import ru.ifmo.android_2015.db.CityDBHelper;
import ru.ifmo.android_2015.lesson6.worldcam.R;

/**
 * Created by dmitry.trunin on 17.11.2015.
 */
public class SelectCityActivity extends Activity {

    AutoCompleteTextView autoCompleteTextView;
    SimpleCursorAdapter adapter;

    private static final String [] PROJECTION = {
            CityContract.CityColumns.CITY_ID,
            CityContract.CityColumns.NAME,
            CityContract.CityColumns.COUNTRY,
            CityContract.CityColumns.LATITUDE,
            CityContract.CityColumns.LONGITUDE
    };

    private static final int INDEX_NAME = 1;
    private static final int INDEX_COUNTRY = 2;
    private static final int INDEX_LATITUDE = 3;
    private static final int INDEX_LONGITUDE = 4;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.select_city_activity);
        autoCompleteTextView = (AutoCompleteTextView) findViewById(R.id.autocomplete);

        adapter = new SimpleCursorAdapter(this,
                android.R.layout.simple_list_item_1,
                null,
                new String[] {CityContract.CityColumns.NAME },
                new int[] { android.R.id.text1 },
                0);

        adapter.setCursorToStringConverter(new SimpleCursorAdapter.CursorToStringConverter() {
            @Override
            public CharSequence convertToString(Cursor cursor) {
                String name = cursor.getString(INDEX_NAME);
                String country = cursor.getString(INDEX_COUNTRY);
                if (!TextUtils.isEmpty(country)) {
                    name = name + " (" + country + ")";
                }
                return name;
            }
        });

        adapter.setFilterQueryProvider(new FilterQueryProvider() {
            @Override
            public Cursor runQuery(CharSequence query) {
                CityDBHelper dbHelper = CityDBHelper.getInstance(SelectCityActivity.this);
                SQLiteDatabase db = dbHelper.getWritableDatabase();

                String selection = null;
                String[] selectionArgs = null;

                if (!TextUtils.isEmpty(query)) {
                    selection = CityContract.CityColumns.NAME + " LIKE ?";
                    selectionArgs = new String[]{query + "%"};
                }
                return db.query(
                        CityContract.Cities.TABLE,
                        PROJECTION,
                        selection,
                        selectionArgs,
                        null /*groupBy*/,
                        null /*having*/,
                        null /*orderBy*/,
                        null /*limit*/);
            }
        });
        autoCompleteTextView.setAdapter(adapter);

        autoCompleteTextView.setOnItemClickListener(new AdapterView.OnItemClickListener() {
            @Override
            public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
                Cursor cursor = (Cursor) adapter.getItem(position);
                if (cursor != null) {
                    String name = cursor.getString(INDEX_NAME);
                    double latitude = cursor.getDouble(INDEX_LATITUDE);
                    double longitude = cursor.getDouble(INDEX_LONGITUDE);

                    Log.d(TAG, "onItemClick: name=" + name + " lat=" + latitude
                            + " lon=" + longitude);
                }

            }
        });
    }

    private static final String TAG = "SelectCity";
}
