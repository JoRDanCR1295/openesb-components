

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package it.imolinfo.jbi4corba.test.servant;

import it.imolinfo.jbi4corba.test.EchoObjPOA;




/**
 *
 * @author Luca
 */
public class EchoObjImpl extends EchoObjPOA {

    
    public String echoObj(String msg) {
        System.out.println("Echo 1 Response ===>"+msg );
        return msg+"response";
    }

   
}
