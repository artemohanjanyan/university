package ru.ifmo.android_2015.util;

import java.io.IOException;
import java.io.InputStream;

/**
 * Created by dmitry.trunin on 14.11.2015.
 */
public class ObservableInputStream extends InputStream {

    private final InputStream in;
    private final ProgressCallback progressCallback;
    private final long totalSize;

    private long readBytes = 0;
    private int progress = 0;

    public ObservableInputStream(InputStream in, long totalSize, ProgressCallback callback) {
        super();
        this.in = in;
        this.totalSize = totalSize;
        this.progressCallback = callback;
    }

    @Override
    public int available() throws IOException {
        return in.available();
    }

    @Override
    public void close() throws IOException {
        in.close();
    }

    @Override
    public void mark(int readlimit) {
    }

    @Override
    public boolean markSupported() {
        return false;
    }

    @Override
    public int read(byte[] buffer) throws IOException {
        final int count = in.read(buffer);
        readBytes += count;
        updateProgress();
        return count;
    }

    @Override
    public int read(byte[] buffer, int byteOffset, int byteCount) throws IOException {
        final int count = in.read(buffer, byteOffset, byteCount);
        readBytes += count;
        updateProgress();
        return count;
    }

    @Override
    public synchronized void reset() throws IOException {
        throw new IOException();
    }

    @Override
    public long skip(long byteCount) throws IOException {
        final long count = in.skip(byteCount);
        readBytes += count;
        updateProgress();
        return count;
    }

    @Override
    public int read() throws IOException {
        final int value = in.read();
        readBytes++;
        updateProgress();
        return value;
    }

    void updateProgress() {
        if (totalSize <= 0) {
            return;
        }
        int newProgress = Math.min((int) (100 * readBytes / totalSize), 100);
        if (newProgress != progress && progressCallback != null) {
            progressCallback.onProgressChanged(progress);
        }
        progress = newProgress;
    }
}
