package cz.janhrcek.nss;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.List;
import java.util.OptionalDouble;
import java.util.logging.Level;

import com.github.robozonky.strategy.natural.GeneratedStrategyVerifier;
import org.junit.After;
import org.junit.Assert;
import org.junit.Test;
import org.openqa.selenium.chrome.ChromeDriver;
import org.openqa.selenium.logging.LogEntry;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

public class RandomStrategyRenderingTest {

    private final TestApp testApp = new TestApp(new ChromeDriver());

    @Test
    public void randomStrategiesCanBeParsed() throws IOException {
        testApp.open();

        List<Integer> lengths = new ArrayList<>();
        for (int i = 0; i < 1000; i++) {
            String renderedStrategy = testApp.nextStrategy();

            strategyIsParsableByRobozonky(renderedStrategy);

            lengths.add(urlLen(renderedStrategy));

            assertEquals("Strategy must not have validation errors",
                         "[]", testApp.getValidationErrors()
            );

            assertEquals("After JSON Encode/Decode roundtrip the strategy must be the same",
                         "Ok", testApp.getJsonEncodeDecodeResult()
            );
        }

        OptionalDouble average = lengths.stream().mapToInt(Integer::intValue).average();
        System.out.println(average);
        
        List<LogEntry> errorsAndWarnings = testApp.getBrowserConsoleLogs().filter(Level.WARNING);
        errorsAndWarnings.forEach(System.out::println);
        assertTrue("Browser console log must not contain errors", errorsAndWarnings.isEmpty());
    }

    private void strategyIsParsableByRobozonky(String strategy) {
        try {
            GeneratedStrategyVerifier.parseWithAntlr(strategy);
        } catch (Exception e) {
            Assert.fail("----- Failed to parse strategy (seed = " + testApp.getStrategySeed() + ") -----\n"
                                + strategy + "\nException was\n" + e.toString());
        }
    }

    private static int urlLen(String strategy) {
        String lineWithUrl = new BufferedReader(new StringReader(strategy)).lines()
                .filter(line -> line.contains("dummy#")).findFirst().get();
        return lineWithUrl.substring(lineWithUrl.indexOf("dummy#") + "dummy#".length()).length();
    }

    @After
    public void closeDriver() {
        testApp.close();
    }
}
