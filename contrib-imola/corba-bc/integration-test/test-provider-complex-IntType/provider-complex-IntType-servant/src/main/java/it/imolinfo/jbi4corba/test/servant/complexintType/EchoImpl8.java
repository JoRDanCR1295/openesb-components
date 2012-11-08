/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package it.imolinfo.jbi4corba.test.servant.complexintType;

import it.imolinfo.jbi4corba.test.webservice.generator.complexintType.Echo;
import it.imolinfo.jbi4corba.test.webservice.generator.complexintType.Echo2;
import it.imolinfo.jbi4corba.test.webservice.generator.complexintType.Echo8POA;
import it.imolinfo.jbi4corba.test.webservice.generator.complexintType.EchoException;
import it.imolinfo.jbi4corba.test.webservice.generator.complexintType.EchoHelper;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.omg.PortableServer.POAPackage.ServantNotActive;
import org.omg.PortableServer.POAPackage.WrongPolicy;

/**
 *
 * @author Luca
 */
public class EchoImpl8 extends Echo8POA {

    public Echo echo(Echo2 intf) throws EchoException {
        
        EchoImpl e=new EchoImpl();
        org.omg.CORBA.Object ref;
        Echo href=null; 
       
        try {
             ref = this._poa().servant_to_reference(e);
             href = EchoHelper.narrow(ref);
             System.out.println(href.getClass().getInterfaces()[0].toString());
             
        } catch (ServantNotActive ex) {
            Logger.getLogger(EchoIntTypeImpl.class.getName()).log(Level.SEVERE, null, ex);
        } catch (WrongPolicy ex) {
            Logger.getLogger(EchoIntTypeImpl.class.getName()).log(Level.SEVERE, null, ex);
        }
        System.out.println("Servant scatena eccezione");
        
        throw new EchoException(href);
    }

}
