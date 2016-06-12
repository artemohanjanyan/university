package ru.ifmo.ctddev.ohanjanyan.implementor;

import info.kgeorgiy.java.advanced.implementor.ImplerException;

import java.nio.file.Paths;

/**
 * Provides entry point for application.
 */
public class Main {
    /**
     * Entry point for application.
     * <p>
     * Usage:
     * <ul>
     *     <li>{@code java -jar Implementor.jar -jar class-to-implement path-to-jar}</li>
     *     <li>{@code java -jar Implementor.jar class-to-implement}</li>
     * </ul>
     * @param args command line arguments
     */
    public static void main(String[] args) {
        Implementor implementor = new Implementor();
        try {
            if (args[0].equals("-jar")) {
                implementor.implementJar(Class.forName(args[1]), Paths.get(args[2]));
            } else {
                implementor.implement(Class.forName(args[0]), Paths.get("."));
            }
        } catch (IndexOutOfBoundsException e) {
            System.err.println("Usage: java -jar Implementor.jar [<classname> | -jar <classname> <output.jar>]");
        } catch (ClassNotFoundException e) {
            System.err.println("no such class: " + e.getMessage());
        } catch (ImplerException e) {
            System.err.println("error while implementing class: " + e.getMessage());
        }
    }
}
