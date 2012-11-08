
package it.imolinfo.test14;

import java.rmi.Remote;


/**
 * This is the business interface for TestSession enterprise bean.
 */
public interface TestSessionRemoteBusiness extends Remote {
    double getBalance(String code) throws java.rmi.RemoteException;
    
}
