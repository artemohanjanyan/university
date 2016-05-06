package ru.ifmo.android_2015.json;

import android.support.annotation.Nullable;
import android.util.JsonReader;
import android.util.Log;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.Arrays;

/**
 * Created by dmitry.trunin on 16.11.2015.
 */
public class CityJsonReaderParser implements CityJsonParser {

    @Override
    public void parseCities(InputStream in, @Nullable CityParserCallback callback)
            throws IOException {
        JsonReader reader = new JsonReader(new InputStreamReader(in));
        reader.beginArray();

        while (reader.hasNext()) {
            parseCity(reader, callback);
        }
        reader.endArray();
    }

    private void parseCity(JsonReader reader, CityParserCallback callback) throws IOException {
        reader.beginObject();

        long id = Long.MIN_VALUE;
        String cityName = null;
        String country = null;
        double[] latLon = null;

        while (reader.hasNext()) {
            final String name = reader.nextName();
            if (name == null) {
                // Вообще, такого быть не должно, но ВСЕГДА надо проверять на null,
                // перед тем как делать switch по строке -- иначе NPE
                reader.skipValue();
                continue;
            }

            switch (name) {
                case "_id":     id = reader.nextLong(); break;
                case "name":    cityName = reader.nextString(); break;
                case "country": country = reader.nextString(); break;
                case "coord":   latLon = parseCoord(reader); break;

                default:        reader.skipValue(); break;
            }
        }
        reader.endObject();

        if (id == Long.MIN_VALUE || cityName == null || country == null || latLon == null) {
            Log.w(LOG_TAG, "Incomplete city data: id=" + id + " cityName=" + cityName
                    + " country=" + country + " latLon=" + Arrays.toString(latLon));

        } else if (callback != null) {
            callback.onCityParsed(id, cityName, country, latLon[0], latLon[1]);
        }
    }

    private double[] parseCoord(JsonReader reader) throws IOException {
        reader.beginObject();

        double lat = Double.NaN;
        double lon = Double.NaN;

        while (reader.hasNext()) {
            final String name = reader.nextName();
            if (name == null) {
                reader.skipValue();
                continue;
            }

            switch (name) {
                case "lat":     lat = reader.nextDouble(); break;
                case "lon":     lon = reader.nextDouble(); break;

                default:        reader.skipValue(); break;
            }
        }
        reader.endObject();

        // NaN == NaN всегда false
        if (lat == lat && lon == lon) {
            return new double[] { lat, lon };
        }
        Log.w(LOG_TAG, "Incomplete coordinates: lat=" + lat + " lon=" + lon);
        return null;
    }

    private static final String LOG_TAG = "CityJRParser";
}
