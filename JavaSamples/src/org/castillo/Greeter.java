package org.castillo;

import java.util.function.Function;
import java.util.regex.Pattern;

public class Greeter implements Function<String, String> {

    private final String[] parts;

    public Greeter(String format) {
        parts = format.split(Pattern.quote("#{name}"));
    }

    public String apply(String name) {
        return String.join(name, parts);
    }
}
