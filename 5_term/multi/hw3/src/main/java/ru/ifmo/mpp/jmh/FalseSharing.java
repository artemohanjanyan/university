/*
 * Copyright (c) 2014, Oracle America, Inc.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *  * Redistributions of source code must retain the above copyright notice,
 *    this list of conditions and the following disclaimer.
 *
 *  * Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 *  * Neither the name of Oracle nor the names of its contributors may be used
 *    to endorse or promote products derived from this software without
 *    specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF
 * THE POSSIBILITY OF SUCH DAMAGE.
 */

package ru.ifmo.mpp.jmh;

import org.openjdk.jmh.annotations.*;
import java.util.concurrent.TimeUnit;

@BenchmarkMode(Mode.AverageTime)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@Fork(5)
@Warmup(iterations = 5)
@Measurement(iterations = 5)
public class FalseSharing {

     /*
     * One of the unusual thing that can bite you back is false sharing.
     * If two threads access (and possibly modify) the adjacent values
     * in memory, chances are, they are modifying the values on the same
     *
     * Suppose we have two threads:
     *   a) innocuous reader which blindly reads its own field
     *   b) furious writer which updates its own field
     */

    /*
     * BASELINE EXPERIMENT:
     * Because of the false sharing, both reader and writer will experience
     * penalties.
     */

    @State(Scope.Group)
    public static class StateBaseline {
        int readOnly;
        int writeOnly;
    }

    @Benchmark
    @Group("baseline")
    public int reader(StateBaseline s) {
        return s.readOnly;
    }

    @Benchmark
    @Group("baseline")
    public void writer(StateBaseline s) {
        s.writeOnly++;
    }

    /*
     * APPROACH 1: PADDING
     *
     * We can try to alleviate some of the effects with padding.
     * This is not versatile because JVMs can freely rearrange the
     * field order.
     */

    @State(Scope.Group)
    public static class StatePadded {
        int readOnly;
        int p01, p02, p03, p04, p05, p06, p07, p08;
        int p11, p12, p13, p14, p15, p16, p17, p18;
        int writeOnly;
        int q01, q02, q03, q04, q05, q06, q07, q08;
        int q11, q12, q13, q14, q15, q16, q17, q18;
    }

    @Benchmark
    @Group("padded")
    public int reader(StatePadded s) {
        return s.readOnly;
    }

    @Benchmark
    @Group("padded")
    public void writer(StatePadded s) {
        s.writeOnly++;
    }
}
