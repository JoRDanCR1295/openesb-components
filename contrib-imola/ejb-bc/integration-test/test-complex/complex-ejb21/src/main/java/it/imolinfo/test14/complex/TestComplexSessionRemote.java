
package it.imolinfo.test14.complex;

import java.rmi.RemoteException;
import javax.ejb.EJBObject;


/**
 * This is the remote interface for TestComplexSession enterprise bean.
 */
public interface TestComplexSessionRemote extends EJBObject, TestComplexSessionRemoteBusiness {
    
   UserProfile getUserProfile(String code) throws RemoteException;    
   void throwException() throws it.imolinfo.test14.complex.UserProfileException, java.rmi.RemoteException;        
}
