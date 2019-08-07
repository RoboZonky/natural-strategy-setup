package cz.janhrcek.nss;

import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.logging.LogEntries;

import java.io.BufferedReader;
import java.io.Closeable;
import java.io.StringReader;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.file.Paths;

class TestApp implements Closeable {

    private final WebDriver driver;

    TestApp(WebDriver driver) {
        this.driver = driver;
    }

    void open(Deployment d) {
        driver.get(d.getUri().toString());
    }

    String nextStrategy() {
        next();
        return getStrategy();
    }

    String getStrategyHash() {
        String strategyString = getStrategy();
        String lineWithUrl = new BufferedReader(new StringReader(strategyString)).lines()
                .filter(line -> line.contains("dummy#"))
                .findFirst()
                .orElseThrow(() -> new IllegalStateException("Generated strategy didn't contain url hash comment"));
        return lineWithUrl.substring(lineWithUrl.indexOf("dummy#") + "dummy#".length());
    }

    String getValidationErrors() {
        WebElement validationErrors = driver.findElement(By.id("validationErrors"));
        return validationErrors.getText();
    }

    String getJsonEncodeDecodeResult() {
        return driver.findElement(By.id("encodingDecodingResult")).getText();
    }

    private void next() {
        WebElement nextSeedButton = driver.findElement(By.id("nextSeedButton"));
        nextSeedButton.click();
    }

    private String getStrategy() {
        return driver.findElement(By.id("renderedStrategy")).getAttribute("value");
    }

    /**
     * @return seed used to generate the strategy returned by previous call to {@link #nextStrategy()}
     */
    public String getStrategySeed() {
        return driver.findElement(By.id("seed")).getAttribute("value");
    }

    public LogEntries getBrowserConsoleLogs() {
        return driver.manage().logs().get("browser");
    }

    public void close() {
        driver.close();
    }

    enum Deployment {
        CURRENT(Paths.get(System.getProperty("user.dir"), "target/testApp.html").toUri().toString()),
        V1("https://janhrcek.cz/nss-strategy-compat/v1/"),
        V2("https://janhrcek.cz/nss-strategy-compat/v2/");

        private final URI uri;

        Deployment(String uriStr) {
            try {
                this.uri = new URI(uriStr);
            } catch (URISyntaxException use) {
                throw new IllegalArgumentException("Invalid URI", use);
            }
        }

        URI getUri() {
            return uri;
        }
    }
}
