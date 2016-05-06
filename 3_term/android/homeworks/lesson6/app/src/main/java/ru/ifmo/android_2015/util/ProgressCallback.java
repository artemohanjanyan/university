package ru.ifmo.android_2015.util;

/**
 * Callback интерфейс для получения уведомления о прогрессе.
 */
public interface ProgressCallback {

    /**
     * Вызывается при изменении значения прогресса.
     * @param progress новое значение прогресса от 0 до 100.
     */
    void onProgressChanged(int progress);
}
