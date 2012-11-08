
package it.imolinfo.test14;

import java.rmi.RemoteException;
import javax.ejb.CreateException;
import javax.ejb.EJBHome;


/**
 * This is the home interface for TestSession enterprise bean.
 */
public interface TestSessionRemoteHome extends EJBHome {
    
    TestSessionRemote create()  throws CreateException, RemoteException;
    
    
}
