/*******************************************************************************
 *  Copyright (c) 2005, 2006, 2007 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 *******************************************************************************/
package it.imolinfo.jbi4ejb.runtime.ejbproxy;

import it.imolinfo.jbi4ejb.Logger;
import it.imolinfo.jbi4ejb.LoggerFactory;
import it.imolinfo.jbi4ejb.exception.EJBInvokeException;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import org.omg.CORBA.ORB;

/**
 * The Stateless EJBProxy.
 * 
 * @author <a href="mailto:mpiraccini@imolinfo.it">Marco Piraccini</a>
 */
public class StatelessEJBProxy {
    
    /** The Constant LOG. */
    private static final Logger LOG
        = LoggerFactory.getLogger(StatelessEJBProxy.class);    
    
    /** The remote interface class name. */
    @SuppressWarnings("unused")
    private String remoteInterfaceClassName = null;         
    
    /** The remote bean. */
    private Object remoteBean = null;           
                
    /** The ejb ClassLoader. */
    private ClassLoader ejbInvokeClassLoader = null;
    
    /** The ejb ClassLoader. */
    private ORB orb = null;          
    
    /** The my remote interface. */
    @SuppressWarnings("unchecked")
    private Class myRemoteInterface = null;
    
    /**
     * To avoid istantiation with no parameters.
     */
    @SuppressWarnings("unused")
    private StatelessEJBProxy() {}
    
    /**
     * To avoid istantiation with no parameters.
     * 
     * @param remoteInterfaceClassName
     *             The remote interface class name
     * @param remoteBean
     *             The remote bean object
     * @param ejbInvokeClassLoader
     *             The invocation class loader
     * @param orb
     *             The CORBA ORB
     */
    public StatelessEJBProxy(String remoteInterfaceClassName, Object remoteBean, ClassLoader ejbInvokeClassLoader, ORB orb) {
        this.remoteInterfaceClassName = remoteInterfaceClassName;
        this.remoteBean = remoteBean;
        this.ejbInvokeClassLoader = ejbInvokeClassLoader;
        this.orb = orb;                
    }
    

    /**
     * Invoke method on the EJB.
     * 
     * @param methodName
     *            The method to invoke
     * @param params
     *            The params for the method invocation
     * 
     * @return the object
     * 
     * @throws EJBInvokeException
     *             If some problems occurs
     * @throws EJBInvokeException
     *          if some problem in the EJB invocation occurs
     * @throws InvocationTargetException 
     *              If an exception is thrown in the invokation
     * @throws IllegalAccessException 
     *              If there are access problems
     */
    @SuppressWarnings("unchecked")
    public Object invokeMethod(String methodName, Object[] params) throws EJBInvokeException, IllegalAccessException, InvocationTargetException {
        if (LOG.isDebugEnabled()) {
            StringBuffer paramsString = new StringBuffer();
            for (int i = 0; i < params.length; i++) {
                paramsString.append("[" + params[i] + "]");                
            }
            String msg = "EJB Invocation: interface:[" + remoteInterfaceClassName + "], method:[" + methodName         + "]"
                +"params:" + paramsString ;
            LOG.debug(msg);
        }
        return EJBProxyUtils.invokeMethod(remoteBean, methodName, params, ejbInvokeClassLoader, orb);
    }
    
    /**
     * Invoke method on the EJB.
     * 
     * @param params
     *            The params for the method invocation
     * @param method
     *            The method to invoke
     * @return the object
     *            The return object
     * @throws EJBInvokeException
     *             If some problems occurs in EJB invocation
     * @throws EJBInvokeException
     *          if some problem in the EJB invocation occurs
     * @throws InvocationTargetException 
     *              If an exception is thrown in the invokation
     * @throws IllegalAccessException 
     *              If there are access problems
     */
    @SuppressWarnings("unchecked")
    public Object invokeMethod(Method method, Object[] params) throws EJBInvokeException,  IllegalAccessException, InvocationTargetException {
        
        if (LOG.isDebugEnabled()) {
            StringBuffer paramsString = new StringBuffer();
            for (int i = 0; i < params.length; i++) {
                paramsString.append("[" + params[i] + "]");                
            }
            String msg = "EJB Invocation: interface:[" + remoteInterfaceClassName + "], method:[" + method.getName() + "]"
                +"params:" + paramsString ;
            LOG.debug(msg);
        }      
        return EJBProxyUtils.invokeMethod(remoteBean, method, params, ejbInvokeClassLoader, orb);
    }    

    /**
     * Gets the orb.
     * 
     * @return the orb
     */
    public ORB getOrb() {
        return orb;
    }
    
    /**
     * Sets the remote interface class.
     * 
     * @param myRemoteInterFace
     *          The remote interface
     * 
     * @throws ClassNotFoundException
     */
    @SuppressWarnings("unchecked")
    public void setRemoteInterfaceClass(Class myRemoteInterFace) {
        this.myRemoteInterface =  myRemoteInterFace;        
    }
    
    /**
     * Gets the remote interface class.
     * 
     * @return the remote object interface
     * 
     * @throws ClassNotFoundException
     */
    @SuppressWarnings("unchecked")
    public Class getRemoteInterfaceClass() {
        return myRemoteInterface;        
    }

    /**
     * Gets the remote interface class name.
     * 
     * @return the remote interface class name
     */
    public String getRemoteInterfaceClassName() {
        return remoteInterfaceClassName;
    }

    /**
     * Gets the ejb invoke class loader.
     * 
     * @return the ejb invoke class loader
     */
    public ClassLoader getEjbInvokeClassLoader() {
        return ejbInvokeClassLoader;
    }
    
           
}
