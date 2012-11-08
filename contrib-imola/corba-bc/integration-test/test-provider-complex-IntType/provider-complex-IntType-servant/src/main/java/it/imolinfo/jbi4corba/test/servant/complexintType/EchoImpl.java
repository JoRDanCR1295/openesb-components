

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package it.imolinfo.jbi4corba.test.servant.complexintType;
import it.imolinfo.jbi4corba.test.webservice.generator.complexintType.EchoPOA;



/**
 *
 * @author Luca
 */
public class EchoImpl extends EchoPOA {

    public String echo(String msg) {
       System.out.println("Echo 1 Response ===>"+msg );
       return msg;
    }

   
}
