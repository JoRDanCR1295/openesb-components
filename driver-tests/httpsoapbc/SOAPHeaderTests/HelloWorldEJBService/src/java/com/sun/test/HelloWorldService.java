/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.sun.test;

import javax.jws.WebMethod;
import javax.jws.WebService;
import javax.ejb.Stateless;

/**
 *
 * @author sweng
 */
@WebService()
@Stateless()
public class HelloWorldService {

    /**
     * Web service operation
     */
    @WebMethod(operationName = "sayHello")
    public String sayHello(String param) {
        //TODO write your implementation code here:
        return "Hello world " + param;
    }

}
