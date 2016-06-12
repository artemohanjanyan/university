package ru.ifmo.ctddev.ohanjanyan.implementor;

import info.kgeorgiy.java.advanced.implementor.Impler;
import info.kgeorgiy.java.advanced.implementor.ImplerException;
import info.kgeorgiy.java.advanced.implementor.JarImpler;

import javax.tools.JavaCompiler;
import javax.tools.ToolProvider;
import java.io.*;
import java.lang.reflect.*;
import java.nio.file.*;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.*;
import java.util.jar.Attributes;
import java.util.jar.JarEntry;
import java.util.jar.JarOutputStream;
import java.util.jar.Manifest;
import java.util.stream.Collectors;

/**
 * Provides implementation for interfaces {@link Impler} and {@link JarImpler}.
 */
public class Implementor implements Impler, JarImpler {
    /**
     * Creates new instance of {@code Implementor}.
     */
    public Implementor() {
    }

    /**
     * Produces code implementing class or interface specified by provided <tt>token</tt>.
     * <p>
     * Generated class full name should be same as full name of the type token with <tt>Impl</tt> suffix
     * added. Generated source code should be placed in the correct subdirectory of the specified
     * <tt>root</tt> directory and have correct file name. For example, the implementation of the
     * interface {@link java.util.List} should go to <tt>$root/java/util/ListImpl.java</tt>
     *
     *
     * @param token type token to create implementation for.
     * @param root root directory.
     * @throws info.kgeorgiy.java.advanced.implementor.ImplerException when implementation cannot be
     * generated.
     */
    @Override
    public void implement(Class<?> token, Path root) throws ImplerException {
        if (token.isPrimitive() || token.isArray()) {
            throw new ImplerException("token should be a class or an interface");
        }
        if (Modifier.isFinal(token.getModifiers())) {
            throw new ImplerException("can't implement final class");
        }

        String packageStr = null;
        if (token.getPackage() != null) {
            packageStr = token.getPackage().getName();
        }

        String classStr = token.getSimpleName() + "Impl";
        Path folderPath = root.resolve(getPackagePath(token));
        Path filePath = folderPath.resolve(classStr + ".java");
        try {
            Files.createDirectories(folderPath);
        } catch (IOException e) {
            throw new ImplerException(e);
        }

        try (PrintWriter printWriter = new PrintWriter(new UnicodeWriter(Files.newBufferedWriter(filePath)))) {
            // Package
            if (packageStr != null) {
                printLine(printWriter, "package", packageStr + ";");
                printWriter.println();
            }

            // Class signature
            String classPrefix = Modifier.toString(token.getModifiers() & ~(Modifier.INTERFACE | Modifier.ABSTRACT));
            printLine(printWriter,
                    classPrefix, "class", classStr,
                    token.isInterface() ? "implements" : "extends",
                    token.getSimpleName(),
                    "{");

            // Constructors
            boolean hasNonPrivateConstructor = false;
            for (Constructor<?> constructor : token.getDeclaredConstructors()) {
                if (Modifier.isPrivate(constructor.getModifiers())) {
                    continue;
                }
                hasNonPrivateConstructor = true;

                printExecutableHeader(printWriter, classStr, constructor);

                // Body
                printWriter.println(" {");
                printWriter.print(INDENT + INDENT + "super(");
                for (int i = 0; i < constructor.getParameterCount(); ++i) {
                    if (i != 0) {
                        printWriter.print(", ");
                    }
                    printWriter.print("arg" + i);
                }
                printWriter.println(");");
                printWriter.println(INDENT + "}");
            }

            if (!token.isInterface() && !hasNonPrivateConstructor) {
                throw new ImplerException("no public or protected constructor");
            }

            // Methods
            HashSet<MethodWrapper> methods = new HashSet<>();
            for (Method method : token.getMethods()) {
                methods.add(new MethodWrapper(method));
            }
            Class<?> token1 = token;
            while (token1 != null && !token1.equals(Object.class)) {
                for (Method method : token1.getDeclaredMethods()) {
                    methods.add(new MethodWrapper(method));
                }
                token1 = token1.getSuperclass();
            }

            for (MethodWrapper method : methods) {
                if (!Modifier.isAbstract(method.method.getModifiers())) {
                    continue;
                }

                printExecutableHeader(printWriter, method.method.getName(), method.method);

                // Body
                printWriter.println(" {");
                if (method.method.getReturnType() != void.class) {
                    printWriter.print(INDENT + INDENT + "return ");
                    printWriter.println(defaults.getOrDefault(method.method.getReturnType(), "null") + ";");
                }
                printWriter.println(INDENT + "}");
            }

            printWriter.println("}");
        } catch (IOException e) {
            throw new ImplerException(e);
        }
    }

    /**
     * Produces <tt>.jar</tt> file implementing class or interface specified by provided <tt>token</tt>.
     * <p>
     * Generated class full name should be same as full name of the type token with <tt>Impl</tt> suffix
     * added.
     *
     * @param token type token to create implementation for.
     * @param jarFile target <tt>.jar</tt> file.
     * @throws ImplerException when implementation cannot be generated.
     */
    @Override
    public void implementJar(Class<?> token, Path jarFile) throws ImplerException {
        // Create temporary folder for intermediate files
        Path tmpPath = Paths.get(System.getProperty("user.home"));
        tmpPath = tmpPath.resolve(".implementor");
        try {
            Files.createDirectories(tmpPath);
        } catch (IOException e) {
            throw new ImplerException("no permission to write to $HOME", e);
        }

        implement(token, tmpPath);

        Path folder = tmpPath.resolve(getPackagePath(token));
        Path sourceFile = folder.resolve(token.getSimpleName() + "Impl.java");
        Path classFile = folder.resolve(token.getSimpleName() + "Impl.class");

        // Compile
        JavaCompiler compiler = ToolProvider.getSystemJavaCompiler();
        int exitCode = compiler.run(null, null, null,
                sourceFile.toString(), "-cp", System.getProperty("java.class.path"));
        if (exitCode != 0) {
            throw new ImplerException("compilation error");
        }

        // Pack jar
        Manifest manifest = new Manifest();
        manifest.getMainAttributes().put(Attributes.Name.MANIFEST_VERSION, "1.0");
        try (JarOutputStream jarOutputStream = new JarOutputStream(Files.newOutputStream(jarFile), manifest);
             BufferedInputStream inputStream = new BufferedInputStream(new FileInputStream(classFile.toString()))) {

            String classZipPath = getPackagePath(token) + "/" + token.getSimpleName() + "Impl.class";
            jarOutputStream.putNextEntry(new JarEntry(classZipPath));

            byte[] buffer = new byte[1024];
            int read;
            while ((read = inputStream.read(buffer)) != -1) {
                jarOutputStream.write(buffer, 0, read);
            }

            jarOutputStream.closeEntry();
        } catch (IOException e) {
            throw new ImplerException(e);
        } finally {
            // Delete temporary files
            try {
                Files.walkFileTree(tmpPath, new SimpleFileVisitor<Path>() {
                    @Override
                    public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) throws IOException {
                        Files.delete(file);
                        return FileVisitResult.CONTINUE;
                    }

                    @Override
                    public FileVisitResult postVisitDirectory(Path dir, IOException exc) throws IOException {
                        Files.delete(dir);
                        return FileVisitResult.CONTINUE;
                    }
                });
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
    }

    /**
     * Generates string representation of relative path to folder where class source should be placed
     * based on it's package.
     * <p>
     * For instance, {@code getPackagePath(java.util.List.class)} will produce "java/util".
     * @param token type token to find location for.
     * @return determined relative path.
     */
    private String getPackagePath(Class<?> token) {
        return token.getPackage() != null ? token.getPackage().getName().replace('.', File.separatorChar) : "";
    }

    /**
     * Prints header of {@link Executable}.
     * <p>
     *     Prints return type of executable if it is a method, name of executable,
     *     parameters it takes (with fully qualified type names) and exceptions it can throw.
     * </p>
     * @param printWriter output destination.
     * @param executableName name of executable to be printed.
     * @param executable executable whose header is printed.
     * @throws IOException error during I/O.S
     */
    private void printExecutableHeader(PrintWriter printWriter, String executableName, Executable executable)
            throws IOException {
        printWriter.println();

        // Signature
        printWriter.print(INDENT +
                Modifier.toString(executable.getModifiers() &
                        ~(Modifier.INTERFACE | Modifier.ABSTRACT | Modifier.TRANSIENT)) +
                " ");
        if (executable instanceof Method) {
            Class<?> returnClass = ((Method) executable).getReturnType();
            printWriter.print(returnClass.getCanonicalName() + " ");
        }
        printWriter.print(executableName);
        printWriter.print("(");

        // Args
        Parameter[] parameters = executable.getParameters();
        printWriter.print(Arrays.stream(parameters)
                .map((param) -> param.getType().getCanonicalName() + " " + param.getName())
                .collect(Collectors.joining(", ")));
        printWriter.print(")");

        // Exceptions
        Class<?> exceptions[] = executable.getExceptionTypes();
        if (exceptions.length > 0) {
            printWriter.print(" throws");
            for (int i = 0; i < exceptions.length; i++) {
                if (i != 0) {
                    printWriter.print(",");
                }
                printWriter.print(" " + exceptions[i].getCanonicalName());
            }
        }
    }

    /**
     * {@link Map} for access to default values of primitive types.
     */
    private static final Map<Class<?>, String> defaults = new HashMap<>();

    /**
     * String which represents one level of indentation (4 spaces as recommended in Java Code Conventions, p. 5)
     * @see "Java Code Conventions"
     */
    private static final String INDENT = "    ";

    static {
        defaults.put(byte.class, "0");
        defaults.put(short.class, "0");
        defaults.put(int.class, "0");
        defaults.put(long.class, "0L");
        defaults.put(float.class, "0.0f");
        defaults.put(double.class, "0.0d");
        defaults.put(char.class, "'\\u0000'");
        defaults.put(boolean.class, "false");
    }

    /**
     * Prints passed strings with single spaces between them.
     * @param printWriter output destination.
     * @param words words to be printed.
     */
    private void printLine(PrintWriter printWriter, String... words) {
        for (int i = 0; i < words.length - 1; i++) {
            printWriter.print(words[i]);
            printWriter.print(' ');
        }
        if (words.length > 0) {
            printWriter.print(words[words.length - 1]);
        }
        printWriter.println();
    }

    /**
     * Wrapper for Method class with custom {@link MethodWrapper#hashCode()} implementation.
     * <p>
     *     {@link MethodWrapper#equals(Object)} and {@link MethodWrapper#hashCode()} implemented here
     *     are used for comparing methods on their signature.
     * </p>
     */
    private class MethodWrapper {
        Method method;

        public MethodWrapper(Method method) {
            this.method = method;
        }

        /**
         * Test of equality of signatures.
         * @param obj {@code MethodWrapper} to be compared to.
         * @return {@code true} if signatures match, {@code false} otherwise.
         */
        @Override
        public boolean equals(Object obj) {
            if (!(obj instanceof MethodWrapper)) {
                return false;
            }
            MethodWrapper that = (MethodWrapper) obj;

            return this.method.getName().equals(that.method.getName()) &&
                    Arrays.equals(method.getParameters(), method.getParameters());
        }

        /**
         * Computes hashcode based on method name and parameters' types only.
         * @return hashcode.
         */
        @Override
        public int hashCode() {
            Parameter[] parameters = method.getParameters();
            int hashCode = Integer.hashCode(parameters.length) + 1;

            for (Parameter parameter : parameters) {
                hashCode = hashCode * 31 + parameter.getType().getCanonicalName().hashCode();
            }
            return hashCode * 31 + method.getName().hashCode();
        }
    }

    /**
     * Filter which writes hex values of non-ASCII characters instead of characters themselves.
     * Used for compatibility with systems, where unicode is not default encoding for javac (Windows).
     */
    private class UnicodeWriter extends FilterWriter {
        /**
         * Create a new filtered writer.
         * @param out a Writer object to provide the underlying stream.
         * @throws NullPointerException if <code>out</code> is <code>null</code>
         */
        public UnicodeWriter(Writer out) {
            super(out);
        }

        /**
         * Writes character if it is within range of ASCII, or {@code \u005CuXXXX},
         * where {@code XXXX} is code of character in hex.
         * @param c {@inheritDoc}
         * @throws IOException {@inheritDoc}
         */
        @Override
        public void write(int c) throws IOException {
            if (c >= 128) {
                super.write(String.format("\\u%04X", c));
            } else {
                super.write(c);
            }
        }
    }
}
