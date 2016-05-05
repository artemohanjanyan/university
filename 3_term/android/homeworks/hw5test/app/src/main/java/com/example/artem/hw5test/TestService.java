package com.example.artem.hw5test;

import android.app.IntentService;
import android.content.Intent;
import android.support.v4.content.LocalBroadcastManager;
import android.util.Log;

/**
 * Created by artem on 12/4/15.
 */
public class TestService extends IntentService {
    public static final String ACTION = "com.example.artem.hw5test.ACTION";
    public static final String STEP = "com.example.artem.hw5test.STEP";

    public TestService() {
        super("test");
        Log.d("test", "created");
    }

    @Override
    protected void onHandleIntent(Intent intent) {
        try {
            Log.d("test", "handled");
            Intent itIntent = new Intent(ACTION);
            for (int i = 0; i < 10; ++i) {
                itIntent.putExtra(STEP, i);
                LocalBroadcastManager.getInstance(this).sendBroadcast(itIntent);

                Log.d("test", Integer.toString(i));
                Thread.sleep(1000);
            }
            Log.d("test", "finished");
            stopSelf();
        } catch (Exception ignored) {
        }
    }
}
