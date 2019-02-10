package cz.janhrcek.nss;

import org.openqa.selenium.WebDriver;
import org.openqa.selenium.chrome.ChromeDriver;
import org.openqa.selenium.chrome.ChromeOptions;

class WebDriverFactory {
    public static WebDriver createWebDriver(boolean headless) {
        ChromeOptions options = new ChromeOptions();
        options.setHeadless(headless);
        return new ChromeDriver(options);
    }
}
