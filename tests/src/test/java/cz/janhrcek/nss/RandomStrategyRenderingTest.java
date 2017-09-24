package cz.janhrcek.nss;

import com.github.robozonky.strategy.natural.GeneratedStrategyVerifier;
import org.junit.After;
import org.junit.Assert;
import org.junit.Test;
import org.openqa.selenium.chrome.ChromeDriver;

import java.io.IOException;

public class RandomStrategyRenderingTest {

    private final TestApp testApp = new TestApp(new ChromeDriver());

    @Test
    public void randomStrategiesCanBeParsed() throws IOException {
        testApp.open();
        for (int i = 0; i < 1000; i++) {
            String renderedStrategy = testApp.nextStrategy();

            try {
                GeneratedStrategyVerifier.parseWithAntlr(renderedStrategy);
            } catch (Exception e) {
                Assert.fail("----- Failed to parse strategy (seed = " + testApp.getStrategySeed() + ") -----\n"
                        + renderedStrategy + "\nException was\n" + e.toString());
            }
        }
    }

    @After
    public void closeDriver() {
        testApp.close();
    }
}
