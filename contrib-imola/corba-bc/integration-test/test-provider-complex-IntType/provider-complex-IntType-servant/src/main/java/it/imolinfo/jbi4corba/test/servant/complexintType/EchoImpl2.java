package it.imolinfo.jbi4corba.test.servant.complexintType;

import it.imolinfo.jbi4corba.test.webservice.generator.complexintType.Echo;
import it.imolinfo.jbi4corba.test.webservice.generator.complexintType.Echo2POA;

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
import it.imolinfo.jbi4corba.test.webservice.generator.complexintType.EchoStruct;





/**
 *
 * @author Luca
 */
public class EchoImpl2 extends Echo2POA {

    public Echo echo(EchoStruct msg) {
        System.out.println(msg.persona.cognome);
        System.out.println(msg.persona.nome);
        
        System.out.println(msg.fieldEcho2.echo("Prova Ivocazione"));
        
        
        return msg.fieldEcho2;
    }

    

}
