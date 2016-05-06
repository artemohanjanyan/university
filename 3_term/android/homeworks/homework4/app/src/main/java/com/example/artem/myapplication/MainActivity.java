package com.example.artem.myapplication;

import android.hardware.Sensor;
import android.hardware.SensorEvent;
import android.hardware.SensorEventListener;
import android.hardware.SensorManager;
import android.support.v7.app.AppCompatActivity;
import android.os.Bundle;
import android.util.Log;
import android.view.Display;
import android.view.Surface;

public class MainActivity extends AppCompatActivity implements SensorEventListener {
    private CompassView compassView;

    private Display display;
    private SensorManager sensorManager;
    private Sensor rotationSensor;


    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);

        compassView = (CompassView) findViewById(R.id.compassView);
        display = getWindowManager().getDefaultDisplay();

        sensorManager = (SensorManager) getSystemService(SENSOR_SERVICE);
        // Not deprecated!
        rotationSensor = sensorManager.getDefaultSensor(Sensor.TYPE_ROTATION_VECTOR);
    }

    @Override
    protected void onResume() {
        super.onResume();

        sensorManager.registerListener(this, rotationSensor, SensorManager.SENSOR_DELAY_GAME);
    }

    @Override
    protected void onPause() {
        super.onPause();

        sensorManager.unregisterListener(this);
    }

    private double displayRotation;
    private double[] rotations = {0, Math.PI / 2, Math.PI, Math.PI * 3 / 2};

    private float[] rotationMatrix = new float[16];
    private float[] orientation = new float[3];
    @Override
    public void onSensorChanged(SensorEvent event) {
        SensorManager.getRotationMatrixFromVector(rotationMatrix, event.values);
        SensorManager.getOrientation(rotationMatrix, orientation);

        switch (display.getRotation()) {
            case Surface.ROTATION_0:
                displayRotation = rotations[0];
                break;
            case Surface.ROTATION_90:
                displayRotation = rotations[1];
                break;
            case Surface.ROTATION_180:
                displayRotation = rotations[2];
                break;
            case Surface.ROTATION_270:
                displayRotation = rotations[3];
                break;
        }

        compassView.setAngle(Math.PI / 2 + orientation[0] + displayRotation);
    }

    @Override
    public void onAccuracyChanged(Sensor sensor, int accuracy) {
    }
}
