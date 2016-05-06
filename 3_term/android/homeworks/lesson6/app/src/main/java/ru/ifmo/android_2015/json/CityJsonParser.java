package ru.ifmo.android_2015.json;

import android.support.annotation.Nullable;

import java.io.InputStream;

/**
 * Created by dmitry.trunin on 16.11.2015.
 */
public interface CityJsonParser {

    void parseCities(InputStream in, @Nullable CityParserCallback callback) throws Exception;
}
