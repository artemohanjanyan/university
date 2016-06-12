#! /bin/bash
java -cp ./java-advanced-2016/artifacts/WebCrawlerTest.jar:./homework8/out/artifacts/homework8_jar/*:./java-advanced-2016/lib/* \
	info.kgeorgiy.java.advanced.crawler.Tester hard ru.ifmo.ctddev.ohanjanyan.crawler.WebCrawler "$1"
