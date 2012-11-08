
package it.imolinfo.test14.complex;

import java.rmi.RemoteException;
import javax.ejb.CreateException;
import javax.ejb.EJBHome;


/**
 * This is the home interface for TestComplexSession enterprise bean.
 */
public interface TestComplexSessionRemoteHome extends EJBHome {
    
    TestComplexSessionRemote create()  throws CreateException, RemoteException;
    
    
}
