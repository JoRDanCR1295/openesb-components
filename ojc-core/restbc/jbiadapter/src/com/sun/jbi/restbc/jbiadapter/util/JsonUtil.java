package com.sun.jbi.restbc.jbiadapter.util;

import java.io.StringWriter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.codehaus.jackson.map.ObjectMapper;
import org.codehaus.jackson.map.type.CollectionType;
import org.codehaus.jackson.map.type.MapType;
import org.codehaus.jackson.map.type.TypeFactory;
import org.codehaus.jackson.type.JavaType;

/**
 * JsonUtil.java
 *
 * @author Edward Chou
 */
public class JsonUtil {

    private final static Logger logger = Logger.getLogger(JsonUtil.class.getName());
    
    private final static ObjectMapper mapper = new ObjectMapper();
    private final static JavaType mapType = MapType.typed(HashMap.class, TypeFactory.fromClass(String.class), TypeFactory.fromClass(String.class));
    private final static JavaType listType = CollectionType.typed(ArrayList.class, TypeFactory.fromClass(String.class));
    
    public static Map<String, String> parseJsonPairs(String s) {
        String input = s;
        if (input.trim().length() == 0) {
            input = "{}";
        }
        
        try {
            return mapper.readValue(input, mapType);
        } catch (Exception e) {
            logger.log(Level.WARNING, "error parsing pairs: " + s + ", default to empty Map<String, String>", e);
            return new HashMap<String, String> ();
        }
    }
    
    public static List<String> parseJsonList(String s) {
        String input = s;
        if (input.trim().length() == 0) {
            input = "[]";
        }
        
        try {
            return mapper.readValue(input, listType);
        } catch (Exception e) {
            logger.log(Level.WARNING, "error parsing list: " + s + ", default to empty List<String>", e);
            return new ArrayList<String> ();
        }
    }

    public static String buildJson(Object o) {
        StringWriter writer = new StringWriter();
        try {
            mapper.writeValue(writer, o);
        } catch (Exception e) {
            logger.log(Level.WARNING, "error building json: " + o + ", default to empty String", e);
        }
        
        return writer.toString();
    }
    
    /*
    // test code
    public static void main(String[] args) {
        try {
            // test parsing pairs
            String inputStr = "";
            System.out.println("result of parsing " + inputStr + " is: " + parseJsonPairs(inputStr));
            
            inputStr = "{}";
            System.out.println("result of parsing " + inputStr + " is: " + parseJsonPairs(inputStr));
            
            inputStr = "{ \"key1\" : \"value1\"}";
            System.out.println("result of parsing " + inputStr + " is: " + parseJsonPairs(inputStr));
            
            inputStr = "{ \"key1\" : \"value1\", \"key2\" : \"value2\" }";
            System.out.println("result of parsing " + inputStr + " is: " + parseJsonPairs(inputStr));
            
            // test building pairs
            Map<String, String> m = new HashMap<String, String> ();
            System.out.println("result of building " + m + " is: " + buildJson(m));
            
            m.put("key1", "value1");
            System.out.println("result of building " + m + " is: " + buildJson(m));
            
            m.put("key2", "value2");
            System.out.println("result of building " + m + " is: " + buildJson(m));
            
            // test parsing list
            inputStr = "";
            System.out.println("result of parsing " + inputStr + " is: " + parseJsonList(inputStr));
            
            inputStr = "[]";
            System.out.println("result of parsing " + inputStr + " is: " + parseJsonList(inputStr));
            
            inputStr = "[ \"value1\"]";
            System.out.println("result of parsing " + inputStr + " is: " + parseJsonList(inputStr));
            
            inputStr = "[ \"value1\", \"value2\", \"value3\" ]";
            System.out.println("result of parsing " + inputStr + " is: " + parseJsonList(inputStr));
            
            // test building list
            List<String> l = new ArrayList<String> ();
            System.out.println("result of building " + l + " is: " + buildJson(l));
            
            l.add("value1");
            System.out.println("result of building " + l + " is: " + buildJson(l));
            
            l.add("value2");
            System.out.println("result of building " + l + " is: " + buildJson(l));
            
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
    */
    
}
