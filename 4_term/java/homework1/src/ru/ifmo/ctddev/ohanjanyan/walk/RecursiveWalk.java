package ru.ifmo.ctddev.ohanjanyan.walk;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.*;
import java.nio.file.attribute.BasicFileAttributes;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

public class RecursiveWalk {
    public static void main(String[] args) {
        try (
                BufferedReader reader = Files.newBufferedReader(Paths.get(args[0]), StandardCharsets.UTF_8);
                BufferedWriter writer = Files.newBufferedWriter(Paths.get(args[1]), StandardCharsets.UTF_8)
        ) {
            String pathStr;
            while ((pathStr = reader.readLine()) != null) {
                Path path = Paths.get(pathStr);
                Files.walkFileTree(path, new SimpleFileVisitor<Path>() {
                    @Override
                    public FileVisitResult visitFile(Path file, BasicFileAttributes attrs)
                            throws IOException {
                        printFile(file);
                        return FileVisitResult.CONTINUE;
                    }

                    @Override
                    public FileVisitResult visitFileFailed(Path file, IOException exc)
                            throws IOException {
                        printFile(file);
                        return FileVisitResult.CONTINUE;
                    }

                    private void printFile(Path file) throws IOException {
                        try (InputStream inputStream = Files.newInputStream(file)) {
                            byte[] hash = computeHash(inputStream);
                            writer.write(hashToString(hash));
                            writer.write(" ");
                            writer.write(file.toString());
                            writer.newLine();
                        } catch (IOException e) {
                            writer.write("00000000000000000000000000000000 ");
                            writer.write(file.toString());
                            writer.newLine();
                        }
                    }
                });
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private static MessageDigest messageDigest;

    static {
        try {
            messageDigest = MessageDigest.getInstance("MD5");
        } catch (NoSuchAlgorithmException ignored) {
        }
    }

    private static byte[] computeHash(InputStream inputStream) throws IOException {
        messageDigest.reset();
        byte[] buffer = new byte[1024];
        int length;
        while ((length = inputStream.read(buffer, 0, 1024)) != -1) {
            messageDigest.update(buffer, 0, length);
        }
        return messageDigest.digest();
    }

    private static String hashToString(byte[] hash) {
        StringBuilder stringBuilder = new StringBuilder();
        for (byte x : hash) {
            stringBuilder.append(String.format("%02X", x));
        }
        return stringBuilder.toString();
    }
}
