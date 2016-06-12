package ru.ifmo.ctddev.ohanjanyan.crawler;

import info.kgeorgiy.java.advanced.crawler.*;

import java.io.IOException;
import java.net.MalformedURLException;
import java.util.*;
import java.util.concurrent.CountDownLatch;

/**
 * Class provides implementation of {@link Crawler} interface.
 */
public class WebCrawler implements Crawler {
    private Downloader downloader;
    private int perHost;

    private ThreadPool downloaders, extractors;

    private final Map<String, HostInfo> hosts = new HashMap<>();

    /**
     * Creates an instance of {@link WebCrawler}.
     * @param downloader {@link Downloader} used to download documents.
     * @param downloaderN maximum number of helper threads created to download documents.
     * @param extractorN maximum number of helper threads created to extract links documents.
     * @param perHost maximum number of documents downloaded from one host simultaneously.
     */
    public WebCrawler(Downloader downloader, int downloaderN, int extractorN, int perHost) {
        this.downloader = downloader;
        this.perHost = perHost;

        downloaders = new ThreadPool(downloaderN);
        extractors = new ThreadPool(extractorN);
    }

    /**
     * Recursively downloads documents at specified depth.
     * @param url URL of starting document.
     * @param depth maximum depth to download documents at.
     * @return {@link Result}, which contains links to all downloaded documents, and pairs of
     * document URLs and {@link IOException}'s thrown during processing of those documents.
     */
    @Override
    public Result download(String url, int depth) {
        final List<String> downloaded = new ArrayList<>();
        final Map<String, IOException> errors = new HashMap<>();

        Queue<String> queue = new ArrayDeque<>();
        queue.add(url);
        final Set<String> enqueued = new HashSet<>();
        enqueued.add(url);

        // Process each depth level separately
        for (int depthI = 0; depthI < depth; ++depthI) {
            final int currentDepth = depthI;
            final Queue<String> currentQueue = queue;
            final Queue<String> nextQueue = new ArrayDeque<>();
            CountDownLatch countDown = new CountDownLatch(currentQueue.size());

            for (String nextUrl : currentQueue) {
                String host;
                try {
                    host = URLUtils.getHost(nextUrl);
                    synchronized (hosts) {
                        //noinspection Java8CollectionsApi
                        if (hosts.get(host) == null) {
                            hosts.put(host, new HostInfo());
                        }
                    }
                } catch (MalformedURLException e) {
                    //noinspection ThrowableResultOfMethodCallIgnored
                    errors.put(nextUrl, e);
                    continue;
                }

                final HostInfo hostInfo = getHost(host);

                class TaskFactory {
                    // called only while owning hostInfo's monitor
                    private Runnable produce() {
                        String currentUrl = hostInfo.queue.remove();
                        return () -> {
                            // Download document
                            Document document;
                            try {
                                document = downloader.download(currentUrl);
                                synchronized (downloaded) {
                                    downloaded.add(currentUrl);
                                }
                            } catch (IOException e) {
                                synchronized (errors) {
                                    //noinspection ThrowableResultOfMethodCallIgnored
                                    errors.put(currentUrl, e);
                                }
                                countDown.countDown();
                                return;
                            } finally {
                                synchronized (hostInfo) {
                                    --hostInfo.runningThreads;
                                    if (hostInfo.queue.size() > 0) {
                                        ++hostInfo.runningThreads;
                                        downloaders.pushTask(this.produce());
                                    }
                                }
                            }

                            if (currentDepth == depth - 1) {
                                countDown.countDown();
                            } else {
                                // Extract links
                                extractors.pushTask(() -> {
                                    List<String> links;
                                    try {
                                        links = document.extractLinks();
                                    } catch (IOException e) {
                                        synchronized (errors) {
                                            //noinspection ThrowableResultOfMethodCallIgnored
                                            errors.put(currentUrl, e);
                                        }
                                        countDown.countDown();
                                        return;
                                    }

                                    for (String link : links) {
                                        boolean flag = false;
                                        synchronized (enqueued) {
                                            flag = enqueued.add(link);
                                        }
                                        if (flag) {
                                            synchronized (nextQueue) {
                                                nextQueue.add(link);
                                            }
                                        }
                                    }

                                    countDown.countDown();
                                });
                            }
                        };
                    }
                }

                synchronized (hostInfo) {
                    hostInfo.queue.add(nextUrl);
                    if (hostInfo.runningThreads == perHost) {
                        continue;
                    }
                    ++hostInfo.runningThreads;
                    downloaders.pushTask(new TaskFactory().produce());
                }
            }

            try {
                countDown.await();
            } catch (InterruptedException ignored) {
            }

            queue = nextQueue;
        }

        return new Result(downloaded, errors);
    }

    /**
     * Closes all helper threads created.
     */
    @Override
    public void close() {
        try {
            downloaders.close();
        } catch (InterruptedException ignored) {
        }
        try {
            extractors.close();
        } catch (InterruptedException ignored) {
        }
    }

    private HostInfo getHost(String host) {
        synchronized (hosts) {
            return hosts.get(host);
        }
    }

    private class HostInfo {
        final Queue<String> queue = new ArrayDeque<>();
        volatile int runningThreads = 0;
    }
}
