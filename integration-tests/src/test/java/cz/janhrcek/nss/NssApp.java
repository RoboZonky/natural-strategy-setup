package cz.janhrcek.nss;

import org.assertj.core.api.Assertions;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

import java.io.Closeable;
import java.net.MalformedURLException;
import java.net.URL;

class NssApp implements Closeable {

    private final WebDriver driver;

    NssApp(WebDriver driver) {
        this.driver = driver;
    }

    public void open() {
        driver.get(getBaseUri());
    }

    public void open(String strategyHash) {
        driver.get("about:blank"); // Open blank page, because strategy hash is only parsed on initialization
        driver.navigate().to(getBaseUri() + "#" + strategyHash);
    }

    private String getBaseUri() {
        return "http://127.0.0.1:3000/index.html";
    }

    public void close() {
        driver.close();
    }

    public String getStrategyRestoredNotification() {
        return driver.findElement(By.cssSelector("[role=alert]")).getText();
    }

    public String getRenderedStrategy() {
        return driver.findElement(By.tagName("textarea")).getAttribute("value");
    }


    public URL getErrorReportingUrl() {
        String urlStr = driver.findElement(By.cssSelector("[role=alert] a")).getAttribute("href");
        URL url = null;
        try {
            url = new URL(urlStr);
        } catch (MalformedURLException e) {
            Assertions.fail("There should be valid error reporting url when strategy restoration from URL hash failed", e);
        }
        return url;
    }

}
