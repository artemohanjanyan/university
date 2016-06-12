package ru.ifmo.ctddev.ohanjanyan.helloudp;

import info.kgeorgiy.java.advanced.hello.HelloClient;
import info.kgeorgiy.java.advanced.hello.Util;

import java.io.IOException;
import java.net.*;

/**
 * Sends request to the server, accepts the results and prints them.
 */
public class HelloUDPClient implements HelloClient {

    /**
     * Entry point for {@link HelloUDPClient}.
     */
    public static void main(String[] args) {

        final int threadN = Integer.parseInt(args[3]);
        final int requestN = Integer.parseInt(args[4]);

        new HelloUDPClient().start(args[0], Integer.parseInt(args[1]), args[2], requestN, threadN);
    }

    /**
     * Creates new instance of HelloUDPClient.
     */
    @SuppressWarnings("WeakerAccess")
    public HelloUDPClient() {
    }

    /**
     * Starts client.
     * @param host name or ip-address computer where server is run.
     * @param port port to send requests to.
     * @param prefix prefix for the requests.
     * @param requestN number of requests in each thread.
     * @param threadN number of threads.
     */
    @Override
    public void start(String host, int port, String prefix, int requestN, int threadN) {
        InetSocketAddress serverAddress = new InetSocketAddress(host, port);
        String format = "%s%d_%d";

        Thread threads[] = new Thread[threadN];

        try {
            for (int i = 0; i < threadN; ++i) {
                final int threadI = i;

                threads[i] = new Thread(() -> {

                    try (DatagramSocket socket = new DatagramSocket(null)) {
                        socket.setSoTimeout(50);

                        for (int requestI = 0; requestI < requestN; ++requestI) {
                            String requestStr = String.format(format, prefix, threadI, requestI);
                            byte message[] = requestStr.getBytes(Util.CHARSET);
                            DatagramPacket request = new DatagramPacket(message, message.length, serverAddress);

                            try {
                                socket.send(request);
                            } catch (IOException ignored) {
                                // We'll try again later
                            }

                            message = new byte[1024];
                            DatagramPacket response = new DatagramPacket(message, message.length);
                            boolean isInterrupted;
                            while (!(isInterrupted = Thread.interrupted())) {
                                try {
                                    socket.receive(response);
                                    String responseStr = new String(
                                            response.getData(),
                                            response.getOffset(),
                                            response.getLength(),
                                            Util.CHARSET);
                                    if (responseStr.substring(7).equals(requestStr)) {
                                        System.out.println(new String(
                                                response.getData(),
                                                response.getOffset(),
                                                response.getLength(),
                                                Util.CHARSET));
                                        break;
                                    }
                                } catch (Exception e) {
                                    try {
                                        socket.send(request);
                                    } catch (IOException ignored) {
                                        // We'll try again later
                                    }
                                }
                            }
                            if (isInterrupted) {
                                return;
                            }
                        }
                    } catch (SocketException e) {
                        System.err.println("Some UDP error, can't start client " + threadI);
                        e.printStackTrace();
                    }
                });

                threads[i].start();
            }

            for (Thread thread : threads) {
                thread.join();
            }
        } catch (InterruptedException e) {
            for (Thread thread1 : threads) {
                thread1.interrupt();
            }
        }
    }
}
