/*
 * EjbClien.java
 * 
 * Created on 27/06/2007, 13:25:04
 * 
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package it.imolinfo.test14.complex;

import javax.naming.InitialContext;

/**
 *
 * @author marco
 */
public class EjbClient {

    public EjbClient() {
    }
    
    public static void main(String[] args) {
        try {
       InitialContext ic = new InitialContext();
        TestComplexSessionRemoteHome home = (TestComplexSessionRemoteHome) ic.lookup("ejb/TestComplexSessionBean");   
        TestComplexSessionRemote remote = home.create();
        remote.throwException();
        } catch (Throwable th) {
            th.printStackTrace();
        }
    }

}
