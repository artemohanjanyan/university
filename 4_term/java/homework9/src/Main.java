import info.kgeorgiy.java.advanced.hello.HelloClient;
import info.kgeorgiy.java.advanced.hello.Util;
import org.junit.Assert;
import ru.ifmo.ctddev.ohanjanyan.helloudp.HelloUDPClient;
import ru.ifmo.ctddev.ohanjanyan.helloudp.HelloUDPServer;

import java.io.IOException;
import java.net.DatagramSocket;
import java.util.concurrent.atomic.AtomicInteger;

import static info.kgeorgiy.java.advanced.hello.HelloClientTest.PREFIX;

/**
 * Launches {@link ru.ifmo.ctddev.ohanjanyan.helloudp.HelloUDPServer}
 * and then launches {@link ru.ifmo.ctddev.ohanjanyan.helloudp.HelloUDPClient},
 * which talks to the server. <br>
 *
 * Used for testing.
 */
public class Main {

    /**
     * Entry point for {@link Main}.
     */
    public static void main(String[] args) throws InterruptedException, IOException {
        HelloUDPServer server = new HelloUDPServer();
        server.start(2539, 2);
        Thread.sleep(500);
        HelloUDPClient client = new HelloUDPClient();
        client.start("127.0.0.1", 2539, "bla", 2, 2);
        server.close();
    }
}
