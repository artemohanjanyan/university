package ru.ifmo.android_2015.json;

import android.support.annotation.Nullable;
import android.util.Log;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Arrays;

/**
 * Created by dmitry.trunin on 16.11.2015.
 */
public class CityJsonObjectParser implements CityJsonParser {

    @Override
    public void parseCities(InputStream in, @Nullable CityParserCallback callback)
            throws IOException, JSONException {
        Log.d(LOG_TAG, "parseCities >>> started parsing...");
        String data = read(in);
        Log.d(LOG_TAG, "parseCities: read full data into buffer");
        JSONArray citiesArray = new JSONArray(data);
        Log.d(LOG_TAG, "parseCities: parsed buffer into JSONArray");

        for (int i = 0; i < citiesArray.length(); i++) {
            JSONObject cityJson = citiesArray.optJSONObject(i);
            if (cityJson != null) {
                parseCity(cityJson, callback);
            }
            if ((i + 1) % 1000 == 0) {
                Log.d(LOG_TAG, "parseCities: parsed " + (i + 1) + " cities");
            }
        }
        Log.d(LOG_TAG, "parseCities <<< done");
    }

    private String read(InputStream in) throws IOException {
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        byte[] buffer = new byte[16 * 1024];
        int bytesRead;

        while ((bytesRead = in.read(buffer)) >= 0) {
            baos.write(buffer, 0, bytesRead);
        }
        baos.close();

        return new String(baos.toByteArray());
    }

    private void parseCity(JSONObject cityJson, CityParserCallback callback) {
        long id = cityJson.optLong("_id", Long.MIN_VALUE);
        String cityName = cityJson.optString("name", null);
        String country = cityJson.optString("country", null);
        JSONObject coordJson = cityJson.optJSONObject("coord");

        if (coordJson == null) {
            Log.w(LOG_TAG, "Missing coordinates");
            return;
        }

        double[] latLon = parseCoord(coordJson);

        if (id == Long.MIN_VALUE || cityName == null || country == null || latLon == null) {
            Log.w(LOG_TAG, "Incomplete city data: id=" + id + " cityName=" + cityName
                    + " country=" + country + " latLon=" + Arrays.toString(latLon));

        } else if (callback != null) {
            callback.onCityParsed(id, cityName, country, latLon[0], latLon[1]);
        }
    }

    private double[] parseCoord(JSONObject coordJson) {
        double lat = coordJson.optDouble("lat", Double.NaN);
        double lon = coordJson.optDouble("lon", Double.NaN);

        // NaN == NaN всегда false
        if (lat == lat && lon == lon) {
            return new double[] { lat, lon };
        }
        Log.w(LOG_TAG, "Incomplete coordinates: lat=" + lat + " lon=" + lon);
        return null;
    }

    private static final String LOG_TAG = "CityJOParser";
}
