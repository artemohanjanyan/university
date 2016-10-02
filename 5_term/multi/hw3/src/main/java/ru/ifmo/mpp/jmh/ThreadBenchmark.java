package ru.ifmo.mpp.jmh;

import org.openjdk.jmh.annotations.Mode;
import org.openjdk.jmh.results.format.ResultFormatType;
import org.openjdk.jmh.runner.Runner;
import org.openjdk.jmh.runner.RunnerException;
import org.openjdk.jmh.runner.options.ChainedOptionsBuilder;
import org.openjdk.jmh.runner.options.Options;
import org.openjdk.jmh.runner.options.OptionsBuilder;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.concurrent.TimeUnit;

public class ThreadBenchmark {

    public static void main(String[] args) throws RunnerException, IOException {

        ChainedOptionsBuilder chainedOptionsBuilder = new OptionsBuilder()
                .mode(Mode.AverageTime)
                .timeUnit(TimeUnit.NANOSECONDS)
                .forks(1)
                .warmupIterations(0)
                .measurementIterations(1)
                .resultFormat(ResultFormatType.TEXT);

        for (int i = 1; i <= 1; ++i) {
            System.out.print(String.format("%d threads.\n\n", i));

            Options options = chainedOptionsBuilder.threads(i).build();
            System.out.println(options.getThreads().get());
            new Runner(options).run();

//            Path source = Paths.get("jmh-result.csv");
//            Path target = Paths.get(String.format("result%d.csv", i));
//            Files.move(source, target);

            System.out.print("\n\n\n");
        }
    }
}
