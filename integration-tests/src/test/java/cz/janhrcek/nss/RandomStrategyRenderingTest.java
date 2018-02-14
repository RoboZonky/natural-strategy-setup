package cz.janhrcek.nss;

import com.github.robozonky.strategy.natural.GeneratedStrategyVerifier;
import org.junit.After;
import org.junit.Assert;
import org.junit.Test;
import org.openqa.selenium.chrome.ChromeDriver;
import org.openqa.selenium.logging.LogEntry;

import java.io.IOException;
import java.util.List;
import java.util.logging.Level;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

public class RandomStrategyRenderingTest {

    private final TestApp testApp = new TestApp(new ChromeDriver());

    @Test
    public void randomStrategiesCanBeParsed() throws IOException {
        testApp.open();
        for (int i = 0; i < 1000; i++) {
            String renderedStrategy = testApp.nextStrategy();

            strategyIsParsableByRobozonky(renderedStrategy);

            assertEquals("Strategy must not have validation errors",
                         "[]", testApp.getValidationErrors()
            );

            assertEquals("After JSON Encode/Decode roundtrip the strategy must be the same",
                         "Ok", testApp.getJsonEncodeDecodeResult()
            );
        }

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

    @After
    public void closeDriver() {
        testApp.close();
    }
}
