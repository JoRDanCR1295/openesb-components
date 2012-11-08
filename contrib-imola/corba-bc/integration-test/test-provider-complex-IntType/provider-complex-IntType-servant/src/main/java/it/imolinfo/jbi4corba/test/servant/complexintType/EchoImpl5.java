/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package it.imolinfo.jbi4corba.test.servant.complexintType;

import it.imolinfo.jbi4corba.test.webservice.generator.complexintType.Echo5POA;
import it.imolinfo.jbi4corba.test.webservice.generator.complexintType.EchoStruct;
import it.imolinfo.jbi4corba.test.webservice.generator.complexintType.EchoStructHolder;

/**
 *
 * @author Luca
 */
public class EchoImpl5 extends Echo5POA {

    public EchoStruct echo(EchoStructHolder echoinout) {
        
        return echoinout.value;
    }

    
}
