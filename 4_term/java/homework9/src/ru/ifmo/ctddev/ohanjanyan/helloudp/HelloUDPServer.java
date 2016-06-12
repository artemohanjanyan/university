package ru.ifmo.ctddev.ohanjanyan.helloudp;

import info.kgeorgiy.java.advanced.hello.HelloServer;
import info.kgeorgiy.java.advanced.hello.Util;

import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.SocketException;

/**
 * Sends tasks, sent by {@link HelloUDPClient} and answers them.
 */
public class HelloUDPServer implements HelloServer {

    private Thread threads[];
    private DatagramSocket socket;

    /**
     * Entry point for {@link HelloUDPServer}.
     */
    public static void main(String[] args) {
        final int port = Integer.parseInt(args[0]);
        final int threadN = Integer.parseInt(args[1]);

        new HelloUDPServer().start(port, threadN);
    }

    @Override
    public void start(int port, int threadN) {
        try {
            socket = new DatagramSocket(port);
            //socket.setSoTimeout(5000);
        } catch (SocketException e) {
            System.err.println("Some UDP error, can't run server.");
            e.printStackTrace();
            return;
        }

        threads = new Thread[threadN];

        for (int i = 0; i < threadN; ++i) {

            threads[i] = new Thread(() -> {
                byte buffer[] = new byte[1024];
                DatagramPacket request = new DatagramPacket(buffer, buffer.length);

                while (!Thread.interrupted()) {
                    try {
                        socket.receive(request);
                    } catch (IOException e) {
                        continue;
                    }

                    byte message[] = ("Hello, " + new String(
                            request.getData(),
                            request.getOffset(),
                            request.getLength(),
                            Util.CHARSET)).getBytes(Util.CHARSET);
                    DatagramPacket response = new DatagramPacket(message, message.length,
                            request.getSocketAddress());

                    try {
                        socket.send(response);
                    } catch (IOException ignored) {
                    }
                }
            });

            threads[i].start();
        }
    }

    @Override
    public void close() {
        if (socket != null) {
            socket.close();
        }
        if (threads != null) {
            for (Thread thread : threads) {
                thread.interrupt();
            }
            try {
                for (Thread thread : threads) {
                    thread.join();
                }
            } catch (InterruptedException ignored) {
            }
        }
    }
}
