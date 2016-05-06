package ru.ifmo.android_2015.util;

import android.content.Context;
import android.util.Log;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;

/**
 * Методы для работы с файлами.
 */
public final class FileUtils {

    /**
     * Создает пустой файл в папке приложения в External Storage
     * Дтректория: /sdcard/Android/data/<application_package_name>/files
     *
     * Имя файла генерируется случайным образом, к нему можно добавить расширение. Файл никак
     * автоматически не удаляется -- получатель сам должен позаботиться об удалении после
     * использования.
     *
     * @param context   контекст приложения
     * @param prefix    начало имени файла
     * @param extension расширение, которое будет добавлено в конце имени файла.
     *
     * @return  новый пустой файл
     *
     * @throws IOException  в случае ошибки создания файла.
     */
    public static File createExternalFile(Context context, String prefix, String extension)
            throws IOException {
        File dir = context.getExternalFilesDir(null);
        if (dir == null) {
            throw new FileNotFoundException("External file dir is null");
        }
        if (dir.exists() && !dir.isDirectory()) {
            throw new IOException("Not a directory: " + dir);
        }
        if (!dir.exists() && !dir.mkdirs()) {
            throw new IOException("Failed to create directory: " + dir);
        }
        return File.createTempFile(prefix, extension, dir);
    }

    public static void cleanExternalFilesDir(Context context) {
        File filesDir = context.getExternalFilesDir(null);
        if (filesDir != null) {
            cleanDir(filesDir);
        }
    }

    public static void cleanDir(File dir) {
        if (!dir.exists() || !dir.isDirectory()) {
            return;
        }
        for (File file : dir.listFiles()) {
            try {
                if (file.isDirectory()) {
                    deleteDir(file);
                } else {
                    if (!file.delete()) {
                        Log.w(TAG, "Failed to delete: " + file);
                    }
                    Log.d(TAG, "Deleted file: " + file);
                }
            } catch (Exception e) {
                Log.w(TAG, "Failed to delete: " + file, e);
            }
        }
    }

    public static void deleteDir(File dir) {
        cleanDir(dir);
        try {
            if (!dir.delete()) {
                Log.w(TAG, "Failed to delete: " + dir);
            }
            Log.d(TAG, "Deleted directory: " + dir);
        } catch (Exception e) {
            Log.w(TAG, "Failed to delete dir: " + dir, e);
        }
    }

    private static final String TAG = "FileUtils";

    private FileUtils() {}
}
