/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.camelse.test.jbi2jbi;

/**
 *
 * @author chikkala
 */
public class OtherPOJO {
    public void print(String body) {
        System.out.println("#### Message received by POJO #####");
        System.out.println(body);
    }
}
