package it.imolinfo.jbi4corba.test.servant;

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */



import it.imolinfo.jbi4corba.test.EchoObj;
import it.imolinfo.jbi4corba.test.EchoObjHelper;
import it.imolinfo.jbi4corba.test.ObjRefPOA;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.omg.CORBA.Object;
import org.omg.PortableServer.POAPackage.ServantNotActive;
import org.omg.PortableServer.POAPackage.WrongPolicy;

/**
 *
 * @author Luca
 */
public class ObjRefImpl extends ObjRefPOA {


    public Object getobjRef(String msg) {
        System.out.println(msg);
        EchoObjImpl e=new EchoObjImpl();
        org.omg.CORBA.Object ref;
        EchoObj href=null;

        try {
             ref = this._poa().servant_to_reference(e);
             href = EchoObjHelper.narrow(ref);
             System.out.println(href.getClass().getInterfaces()[0].toString());

        } catch (ServantNotActive ex) {
            Logger.getLogger(EchoOBjectDPImpl.class.getName()).log(Level.SEVERE, null, ex);
        } catch (WrongPolicy ex) {
            Logger.getLogger(EchoOBjectDPImpl.class.getName()).log(Level.SEVERE, null, ex);
        }
        
        return (Object)href;
    }

}
