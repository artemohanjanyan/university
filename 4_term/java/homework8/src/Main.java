import info.kgeorgiy.java.advanced.crawler.CachingDownloader;
import info.kgeorgiy.java.advanced.crawler.Crawler;
import info.kgeorgiy.java.advanced.crawler.Result;
import ru.ifmo.ctddev.ohanjanyan.crawler.WebCrawler;

import java.io.IOException;

/**
 * Class for manual testing.
 */
public class Main {
    /**
     * Provides entry point for manual testing application.
     * Behaviour is not specified and this method shouldn't be relied on.
     * Use at your own risk.
     * @param args command line arguments.
     * @throws IOException if {@link CachingDownloader}'s constructor throws an exception.
     */
    public static void main(String[] args) throws IOException {
        try (Crawler crawler = new WebCrawler(new CachingDownloader(), 10, 10, 10)) {

            Result links = crawler.download("http://neerc.ifmo.ru/subregions/index.html", 2);

            System.out.println(links.getDownloaded().size());
//            links.getDownloaded().forEach(System.out::println);
//
//            System.out.println();
//            System.out.println("errors");
//            links.getErrors().forEach((url, exception) -> {
//                System.out.println(url + ": " + exception);
//            });
        }
    }
}
