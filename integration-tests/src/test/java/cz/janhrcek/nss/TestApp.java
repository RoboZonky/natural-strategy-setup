package cz.janhrcek.nss;

import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.logging.LogEntries;

import java.io.BufferedReader;
import java.io.StringReader;
import java.nio.file.Path;
import java.nio.file.Paths;

class TestApp {

    private final WebDriver driver;

    TestApp(WebDriver driver) {
        this.driver = driver;
    }

    void open() {
        String testProjectDir = System.getProperty("user.dir");
        Path testAppPath = Paths.get(testProjectDir, "target/testApp.html");
        driver.get(testAppPath.toUri().toString());
    }

    String nextStrategy() {
        next();
        System.out.print(".");
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
}
