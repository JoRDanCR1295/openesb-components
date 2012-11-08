/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package it.imolinfo.jbi4corba.test.servant.complexintType;

import it.imolinfo.jbi4corba.test.webservice.generator.complexintType.Echo7POA;
import it.imolinfo.jbi4corba.test.webservice.generator.complexintType.IntfStruct;
import it.imolinfo.jbi4corba.test.webservice.generator.complexintType.IntfStructHolder;

/**
 *
 * @author Luca
 */
public class EchoImpl7 extends Echo7POA {

    public IntfStruct echo(IntfStructHolder mystruct) {
        
        return mystruct.value;
    }

}
