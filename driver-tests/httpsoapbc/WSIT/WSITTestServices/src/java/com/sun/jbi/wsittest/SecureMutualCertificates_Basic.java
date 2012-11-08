/*
 * SecureMutualCertificates_Basic.java
 * 
 * Created on Aug 17, 2007, 6:17:34 PM
 * 
 * To change this template, choose Tools | Templates
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
public class SecureMutualCertificates_Basic {
    /**
     * Web service operation
     */

    @WebMethod(action="echo")
    public String echo(@WebParam(name = "p1")String p1) {
        return p1;
    }
    
}
