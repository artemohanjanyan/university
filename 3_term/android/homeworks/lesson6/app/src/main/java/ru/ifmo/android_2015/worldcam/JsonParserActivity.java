package ru.ifmo.android_2015.worldcam;

import android.text.TextUtils;
import android.util.Log;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.util.zip.GZIPInputStream;

import ru.ifmo.android_2015.json.CityJsonObjectParser;
import ru.ifmo.android_2015.json.CityJsonParser;
import ru.ifmo.android_2015.json.CityJsonReaderParser;
import ru.ifmo.android_2015.util.ObservableInputStream;

/**
 * Created by dmitry.trunin on 16.11.2015.
 */
public class JsonParserActivity extends ProgressTaskActivity {

    public static final String EXTRA_PARSER_TYPE = "parser";
    public static final String PARSER_JSON_READER = "json_reader";
    public static final String PARSER_JSON_OBJECT = "json_object";

    @Override
    protected ProgressTask createTask() {
        String parserType = getIntent().getStringExtra(EXTRA_PARSER_TYPE);
        if (parserType == null) {
            parserType = PARSER_JSON_READER;
        }

        final CityJsonParser parser;

        if (PARSER_JSON_READER.equals(parserType)) {
            parser = new CityJsonReaderParser();
        } else {
            parser = new CityJsonObjectParser();
        }

        return new JsonParserTask(this, parser);
    }

    static class JsonParserTask extends ProgressTask {

        private final CityJsonParser parser;

        JsonParserTask(ProgressTaskActivity activity, CityJsonParser parser) {
            super(activity);
            this.parser = parser;
        }

        @Override
        protected void runTask() throws Exception {
            String fileName = new WorldcamPreferences(appContext).getCitiesFileName();
            if (TextUtils.isEmpty(fileName)) {
                throw new FileNotFoundException("File name is null");
            }
            File file = new File(appContext.getExternalFilesDir(null), fileName);

            InputStream in = null;

            try {
                long fileSize = file.length();
                in = new FileInputStream(file);
                in = new BufferedInputStream(in);
                in = new ObservableInputStream(in, fileSize, this);
                in = new GZIPInputStream(in);
                parser.parseCities(in, null /*callback*/);

            } finally {
                if (in != null) {
                    try {
                        in.close();
                    } catch (IOException e) {
                        Log.w(LOG_TAG, "Failed to close file: " + file);
                    }
                }
            }
        }
    }
}
