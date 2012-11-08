/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.camelse.test.jbi2pojo;

/**
 * This class is used for processing message on pojo endpoint in a inout 
 * message exchange.
 * @author chikkala
 */
public class MyPOJO {
    public String printAndReply(String body) {
        System.out.println("###### MyPOJO Received message ########");
        System.out.println(body);
        return body.replace("Hello Camel", "Hello JBI");
    }
}
