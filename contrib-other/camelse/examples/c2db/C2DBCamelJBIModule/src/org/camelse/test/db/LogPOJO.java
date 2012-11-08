/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.camelse.test.db;

import java.util.Map;
import org.apache.camel.Body;
import org.apache.camel.Headers;

/**
 *
 * @author chikkala
 */
public class LogPOJO {


    public void print(@Body String body, @Headers Map headers) {
        System.out.println("##### BEGIN: Log POJO processing exchange");
        System.out.println("@@@ Begin: In Message");
        System.out.println(body);
        printHeaders(headers);
        System.out.println("@@@ End: In Message");
        System.out.println("##### END: Log POJO processing exchange");
    }

    private void printHeaders(Map headers) {
        System.out.println("#### BEGIN Message Headers #####");
        for (Object name : headers.keySet()) {
            Object value = headers.get(name);
            System.out.println(name + "=" + value);
            System.out.println(" Value type: " + value.getClass().getName());
        }
        System.out.println("#### END Headers #####");
    }
}
