package net.openesb.jbi.restbc.jbiadapter.util;

import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.ws.rs.core.MediaType;

/**
 * MediaTypeUtil.java
 *
 * @author Edward Chou
 */
public class MediaTypeUtil {

    private static Pattern whitespaceOrQuote = Pattern.compile("[\\s\"]");
    
    public static String mediaTypeToString(MediaType type) throws Exception {
        StringBuilder b = new StringBuilder();
        b.append(type.getType()).
            append('/').
            append(type.getSubtype());
        for (Map.Entry<String, String> e : type.getParameters().entrySet()) {
            b.append(';').
                append(e.getKey()).
                append('=');
            appendQuotedMediaType(b, e.getValue());
        }
        return b.toString();
    }
    
    private static void appendQuotedMediaType(StringBuilder b, String value) {
        if (value==null)
            return;
        Matcher m = whitespaceOrQuote.matcher(value);
        boolean quote = m.find();
        if (quote)
            b.append('"');
        appendEscapingQuotes(b, value);
        if (quote)
            b.append('"');        
    }

    private static void appendEscapingQuotes(StringBuilder b, String value) {
        for (int i = 0; i < value.length(); i++) {
            char c = value.charAt(i);
            if (c == '"')
                b.append('\\');
            b.append(c);
        }
    }
}
