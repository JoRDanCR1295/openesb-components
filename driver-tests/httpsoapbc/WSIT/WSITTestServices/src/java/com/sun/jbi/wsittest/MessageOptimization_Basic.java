/*
 * MessageOptimization_Basic.java
 *
 * Created on May 26, 2007, 5:01 PM
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package com.sun.jbi.wsittest;

import javax.jws.WebMethod;
import javax.jws.WebParam;
import javax.jws.WebService;

/**
 *
 * @author Frank
 */
@WebService()
public class MessageOptimization_Basic {
    /**
     * Web service operation
     */

    @WebMethod(action="echo")
    public String echo(@WebParam(name = "p1")String p1) {
        return p1;
    }
    
}
