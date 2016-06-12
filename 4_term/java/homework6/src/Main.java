import info.kgeorgiy.java.advanced.mapper.ParallelMapper;
import ru.ifmo.ctddev.ohanjanyan.concurrent.IterativeParallelism;
import ru.ifmo.ctddev.ohanjanyan.concurrent.ParallelMapperImpl;

import java.util.Collections;
import java.util.List;

public class Main {
    public static void main(String[] args) throws Exception {
//        IterativeParallelism instance = new IterativeParallelism();
//        System.out.println(instance.maximum(1, Arrays.asList(1, 2, 3, 4, 5), Comparator.naturalOrder()));
//
//        StringBuilder stringBuilder = new StringBuilder(10);
//        stringBuilder.setLength(10);
//        stringBuilder.setCharAt(5, 'a');
//        System.out.println(stringBuilder.toString());

        int threadN = 4;

        ParallelMapper parallelMapper = new ParallelMapperImpl(threadN);
        IterativeParallelism instance = new IterativeParallelism(parallelMapper);
        List<Long> list = Collections.nCopies(10000, 23456789L);

        long startTime = System.nanoTime();
        System.out.println(instance.all(threadN, parallelMapper.map(Main::isPrime, list), (b) -> b));
        System.out.println(instance.all(threadN, instance.map(threadN, list, Main::isPrime), (b) -> b));
        System.out.println((System.nanoTime() - startTime) / 1000000L);
        parallelMapper.close();
    }

    private static boolean isPrime(long x) {
        for (long i = 2; i * i <= x; ++i) {
            if (x % i == 0) {
                return false;
            }
        }
        return true;
    }
}
