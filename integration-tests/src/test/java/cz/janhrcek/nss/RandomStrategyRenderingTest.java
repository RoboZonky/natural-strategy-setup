package cz.janhrcek.nss;

import java.util.ArrayList;
import java.util.List;
import java.util.OptionalDouble;
import java.util.logging.Level;

import com.github.robozonky.strategy.natural.GeneratedStrategyVerifier;
import org.assertj.core.api.Assertions;
import org.assertj.core.data.Offset;
import org.junit.After;
import org.junit.Test;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.chrome.ChromeDriver;
import org.openqa.selenium.chrome.ChromeOptions;
import org.openqa.selenium.logging.LogEntry;

import static org.assertj.core.api.Assertions.assertThat;

public class RandomStrategyRenderingTest {

    private final TestApp testApp = new TestApp(initWebDriver());

    @Test
    public void randomStrategiesCanBeParsed() {
        testApp.open();

        List<Integer> encodedStrategyLengths = new ArrayList<>();
        ProgressBar progressBar = new ProgressBar();
        final int numberOfStategiesToGenerate = 1000;
        for (int i = 1; i <= numberOfStategiesToGenerate; i++) {
            String renderedStrategy = testApp.nextStrategy();

            encodedStrategyLengths.add(testApp.getStrategyHash().length());

            strategyIsParsableByRobozonky(renderedStrategy);

            assertThat(testApp.getValidationErrors())
                    .as("Strategy must not have validation errors")
                    .isEqualTo("[]");

            assertThat(testApp.getJsonEncodeDecodeResult())
                    .as("After JSON Encode/Decode roundtrip the strategy must be the same")
                    .isEqualTo("Ok");

            progressBar.update(i, numberOfStategiesToGenerate);
        }

        OptionalDouble averageLengthOfUrlEncodedStrategy = encodedStrategyLengths.stream().mapToInt(Integer::intValue).average();
        assertThat(averageLengthOfUrlEncodedStrategy)
                .as("Average length of strategy encoded in URL should be around 2100 characters")
                .isNotEmpty()
                .hasValueCloseTo(2100.0, Offset.offset(50.0));

        List<LogEntry> errorsAndWarnings = testApp.getBrowserConsoleLogs().filter(Level.WARNING);
        assertThat(errorsAndWarnings)
                .as("Browser console log must not contain errors")
                .isEmpty();
    }

    private void strategyIsParsableByRobozonky(String strategy) {
        try {
            GeneratedStrategyVerifier.parseWithAntlr(strategy);
        } catch (Exception exception) {
            Assertions.fail("---------- Strategy with seed " + testApp.getStrategySeed() + " could not be parsed by robozonky  ----------\n" + strategy,
                            exception
            );
        }
    }

    @After
    public void closeDriver() {
        testApp.close();
    }

    private WebDriver initWebDriver() {
        ChromeOptions options = new ChromeOptions();
        options.setHeadless(true);
        return new ChromeDriver(options);
    }
}
