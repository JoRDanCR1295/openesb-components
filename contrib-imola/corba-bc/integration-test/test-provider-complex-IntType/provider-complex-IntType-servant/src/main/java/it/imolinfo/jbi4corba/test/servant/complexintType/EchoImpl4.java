/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package it.imolinfo.jbi4corba.test.servant.complexintType;

import it.imolinfo.jbi4corba.test.webservice.generator.complexintType.Echo;
import it.imolinfo.jbi4corba.test.webservice.generator.complexintType.Echo4POA;
import it.imolinfo.jbi4corba.test.webservice.generator.complexintType.EchoHolder;

/**
 *
 * @author Luca
 */
public class EchoImpl4 extends Echo4POA {

    public Echo echo(EchoHolder echoinout) {
            
        System.out.println("Richiesta Inout");
        return echoinout.value;
        
    }

}
