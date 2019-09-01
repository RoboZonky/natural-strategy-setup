package cz.janhrcek.nss;


import com.github.robozonky.strategy.natural.GeneratedStrategyVerifier;
import org.assertj.core.api.Assertions;
import org.assertj.core.util.Strings;
import org.junit.After;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;
import org.junit.runners.Parameterized.Parameter;

import java.io.UnsupportedEncodingException;
import java.net.URL;
import java.net.URLDecoder;
import java.nio.charset.StandardCharsets;
import java.util.*;
import java.util.stream.Collectors;

import static java.util.stream.Collectors.mapping;
import static java.util.stream.Collectors.toList;
import static org.assertj.core.api.Assertions.assertThat;

@RunWith(Parameterized.class)
public class BackwardCompatibilityTest {


    private final TestApp testApp = new TestApp(WebDriverFactory.createHeadlessWebDriver());
    private final NssApp nssApp = new NssApp(WebDriverFactory.createHeadlessWebDriver());

    @Parameter
    public TestApp.Deployment legacyDeployment;

    @Parameters
    public static Iterable<? extends Object> data() {
        return Arrays.asList(
                TestApp.Deployment.V1,
                TestApp.Deployment.V2,
                TestApp.Deployment.V3
        );
    }

    @After
    public void closeApps() {
        testApp.close();
        nssApp.close();
    }

    @Test
    public void canRestoreLegacyStrategies() {
        testApp.open(legacyDeployment);

        ProgressBar progressBar = new ProgressBar();
        final int numberOfStrategies = 100;
        for (int i = 1; i <= numberOfStrategies; i++) {
            String strategyHash = testApp.getStrategyHash();
            nssApp.open(strategyHash);
            String notification = nssApp.getStrategyRestoredNotification();

            if (notification.startsWith("Pokus o načtení strategie z URL se nezdařil")) {
                URL errorReportingUrl = nssApp.getErrorReportingUrl();
                assertThat(errorReportingUrl)
                        .hasProtocol("https")
                        .hasHost("github.com")
                        .hasPath("/RoboZonky/natural-strategy-setup/issues/new")
                        .hasParameter("title")
                        .hasParameter("body");
                Map<String, List<String>> queryParams = splitQuery(errorReportingUrl);
                assertThat(queryParams)
                        .containsKey("title")
                        .containsKey("body");
                assertThat(queryParams.get("body")).hasSize(1);
                String percentEncodedGithubIssueBody = queryParams.get("body").get(0);

                String gitHubIssueBody = percentDecode(percentEncodedGithubIssueBody);
                System.out.println(gitHubIssueBody);
                Assertions.fail("Restoring strategy failed.");
            }

            String restoredStrategy = nssApp.getRenderedStrategy();
            try {
                GeneratedStrategyVerifier.parseWithAntlr(restoredStrategy);
            } catch (Exception exception) {
                Assertions.fail(
                        "---------- Strategy restored from URL can no longer be parsed by Robozonky: " + restoredStrategy,
                        exception
                );
            }

            progressBar.update(i, numberOfStrategies);
            testApp.nextStrategy();
        }

    }


    private String percentDecode(String input) {
        try {
            return URLDecoder.decode(input, StandardCharsets.UTF_8.name());
        } catch (UnsupportedEncodingException e) {
            throw new RuntimeException(e);
        }
    }

    private Map<String, List<String>> splitQuery(URL url) {
        if (Strings.isNullOrEmpty(url.getQuery())) {
            return Collections.emptyMap();
        }
        return Arrays.stream(url.getQuery().split("&"))
                .map(this::splitQueryParameter)
                .collect(Collectors.groupingBy(AbstractMap.SimpleImmutableEntry::getKey, LinkedHashMap::new, mapping(Map.Entry::getValue, toList())));
    }

    private AbstractMap.SimpleImmutableEntry<String, String> splitQueryParameter(String it) {
        final int idx = it.indexOf("=");
        final String key = idx > 0 ? it.substring(0, idx) : it;
        final String value = idx > 0 && it.length() > idx + 1 ? it.substring(idx + 1) : null;
        return new AbstractMap.SimpleImmutableEntry<>(key, value);
    }
}
