package ru.ifmo.android_2015.homework5;

import android.graphics.Bitmap;

/**
 * Created by artem on 11/12/15.
 */
public class Webcam {
    private String name;
    private Bitmap bitmap;

    public Webcam(String name, Bitmap bitmap) {
        this.name = name;
        this.bitmap = bitmap;
    }

    public String getName() {
        return name;
    }

    public Bitmap getBitmap() {
        return bitmap;
    }
}
