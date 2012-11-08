package it.imolinfo.jbi4ejb.test.wsdlgeneration.interfaces;

import java.rmi.Remote;
import java.rmi.RemoteException;

public interface TestInterface extends Remote {
    
    public ComplexReturnType complexMethod(ComplexParameter complex, int code, String stringParameter, Boolean bo) throws ComplexException, RemoteException;
}
