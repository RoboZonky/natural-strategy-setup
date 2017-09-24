package cz.janhrcek.nss;

import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.logging.LogEntries;

import java.nio.file.Path;
import java.nio.file.Paths;

class TestApp {

    private final WebDriver driver;

    public TestApp(WebDriver driver) {
        this.driver = driver;
    }

    public void open() {
        String testProjectDir = System.getProperty("user.dir");
        Path testAppPath = Paths.get(testProjectDir, "target/testApp.html");
        driver.get(testAppPath.toUri().toString());
    }

    public String nextStrategy() {
        WebElement nextSeedButton = driver.findElement(By.id("nextSeedButton"));
        nextSeedButton.click();
        System.out.println("Seed = " + getStrategySeed());
        return driver.findElement(By.id("renderedStrategy")).getAttribute("value");
    }

    /**
     * @return seed used to generate the strategy returned by previous call to {@link #nextStrategy()}
     */
    public String getStrategySeed() {
        return driver.findElement(By.id("seed")).getAttribute("value");
    }

    public void close() {
        LogEntries logs = driver.manage().logs().get("browser");
        logs.forEach(logEntry -> System.out.println(logEntry));
        driver.close();
    }
}
