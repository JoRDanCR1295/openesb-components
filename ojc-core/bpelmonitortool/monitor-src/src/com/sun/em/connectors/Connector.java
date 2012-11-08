/* *************************************************************************
 *
 *          Copyright (c) 2002, SeeBeyond Technology Corporation,
 *          All Rights Reserved
 *
 *          This program, and all the routines referenced herein,
 *          are the proprietary properties and trade secrets of
 *          SEEBEYOND TECHNOLOGY CORPORATION.
 *
 *          Except as provided for by license agreement, this
 *          program shall not be duplicated, used, or disclosed
 *          without  written consent signed by an officer of
 *          SEEBEYOND TECHNOLOGY CORPORATION.
 *
 ***************************************************************************/
package com.sun.em.connectors;

import javax.management.Attribute;
import javax.management.AttributeList;
import javax.management.AttributeNotFoundException;
import javax.management.InstanceAlreadyExistsException;
import javax.management.InstanceNotFoundException;
import javax.management.IntrospectionException;
import javax.management.InvalidAttributeValueException;
import javax.management.ListenerNotFoundException;
import javax.management.MBeanException;
import javax.management.MBeanInfo;
import javax.management.MBeanRegistrationException;
import javax.management.NotCompliantMBeanException;
import javax.management.NotificationFilter;
import javax.management.NotificationListener;
import javax.management.ObjectInstance;
import javax.management.ObjectName;
import javax.management.QueryExp;
import javax.management.MBeanServerConnection;
import javax.management.ReflectionException;
import javax.management.j2ee.Management;

import java.io.IOException;
import java.util.Properties;
import java.util.Set;


/**
 *
 * @author  Yoke Lee
 */


public abstract class Connector implements AbstractConnector {
	
    protected String protocol;
    protected String hostname;
    protected String port;
    protected String username;
    protected String password;		// encrypted or clear-text ??
    protected String jndiName;
    protected Management mgmtConnector;
    protected MBeanServerConnection mbeanServerConnector;
    private int mHostType;
    
    
    /**
     * Connector based 
     * @param hostname -  name of the remote host
     * @param port - port of the host remote host
     * @param username - 
     * @param password - 
     */
    public Connector(String _hostname, String _port, String _username, String _password, int hostType) {
    									
    	this(null,_hostname,_port,_username,_password,null,hostType);
    }
    
    
    public Connector(String _hostname, String _port, String _username, String _password, String _jndiName,
                    int hostType) {
    	this(null,_hostname,_port,_username,_password,_jndiName,hostType);
    }    
    
    public Connector(String _protocol, String _hostname, String _port, String _username, String _password,
                    String _jndiName,int _hostType) {
        protocol = _protocol;
    	hostname = _hostname;
        port = _port;
        username = _username;
        password = _password;
        jndiName = _jndiName;
        mHostType = _hostType;
    }    

    
    
    /**
     * subclass need to implement this method
     * 
     * @throws Exception
     */
    public abstract AbstractConnector getConnector() throws Exception;

    
    /**
     * subclass need t implement this method 
     */
    public abstract AbstractConnector getConnector(Properties env) throws Exception;
    
    
    public Management getManagement() {
    	return mgmtConnector;
    }
    
    public MBeanServerConnection getMBeanServerConnection() {
    	return mbeanServerConnector;
    }
    
    public String getProtocol() {
    	return protocol;
    }
    
    public String getHostName() {
    	return hostname;
    }
    
    public String getPort() {
    	return port;
    }
    
    public String getUserName() {
    	return username;
    }
    
    public String getUserPassword() {
    	return password;
    }
    
    public String getJndiName() {
    	return jndiName;
    }
    
    
    // Public --------------------------------------------------------

    /**
     * @todo Document this method
     *
     * @param pClassName X
     * @param pName X
     * @return X
     * @exception ReflectionException X
     * @exception InstanceAlreadyExistsException X
     * @exception MBeanRegistrationException X
     * @exception MBeanException X
     * @exception NotCompliantMBeanException X
     */
    public ObjectInstance createMBean(
            String className,
            ObjectName objectName
            )
        throws
            ReflectionException,
            InstanceAlreadyExistsException,
            MBeanRegistrationException,
            MBeanException,
            NotCompliantMBeanException,
	      java.io.IOException, 
            java.rmi.RemoteException {
    	
    	throw new MBeanException(new Exception("createMBean method is not supported !!"));
    }


    /**
     * @todo Document this method
     *
     * @param className X
     * @param objectName X
     * @param loaderName X
     * @return X
     * @exception ReflectionException X
     * @exception InstanceAlreadyExistsException X
     * @exception MBeanRegistrationException X
     * @exception MBeanException X
     * @exception NotCompliantMBeanException X
     * @exception InstanceNotFoundException X
     */
    public ObjectInstance createMBean(
            String className,
            ObjectName objectName,
            ObjectName loaderName
            )
        throws
            ReflectionException,
            InstanceAlreadyExistsException,
            MBeanRegistrationException,
            MBeanException,
            NotCompliantMBeanException,
            InstanceNotFoundException, 
		java.io.IOException,
            java.rmi.RemoteException {
    	
    	throw new MBeanException(new Exception("createMBean method is not supported !!"));
    }


    /**
     * Instantiates the given class and registers it on the remote MBeanServer
     * and returns an Object Instance of the MBean.
     *
     * @param className Class name of the class to be loaded and instantiated
     * @param objectNameToAssign Object Name the new MBean should be assigned to
     * @param params Array of parameter passed to the creator of the class. If
     *      one is of data type Object handler it will be replaced on the
     *      server-side by its effective object.
     * @param signature Array of Class Names (full qualified) to find the right
     *      parameter. When there is an ObjectHandler as a parameter type then
     *      it will be replaced on the server- side by the class name of the
     *      effective object) otherwise it will be kept.
     * @exception ReflectionException X
     * @exception InstanceAlreadyExistsException X
     * @exception MBeanRegistrationException X
     * @exception MBeanException X
     * @exception NotCompliantMBeanException X
     * @return Object Instance of the new MBean
     */
    public ObjectInstance createMBean(
            String className,
            ObjectName objectNameToAssign,
            Object[] params,
            String[] signature
            )
        throws
            ReflectionException,
            InstanceAlreadyExistsException,
            MBeanRegistrationException,
            MBeanException,
            NotCompliantMBeanException, 
		java.io.IOException,
            java.rmi.RemoteException {
    	
    	throw new MBeanException(new Exception("createMBean method is not supported !!"));
    }


    /**
     * @todo Document this method
     *
     * @param className X
     * @param objectName X
     * @param loaderName X
     * @param params X
     * @param signature X
     * @return X
     * @exception ReflectionException X
     * @exception InstanceAlreadyExistsException X
     * @exception MBeanRegistrationException X
     * @exception MBeanException X
     * @exception NotCompliantMBeanException X
     * @exception InstanceNotFoundException X
     */
    public ObjectInstance createMBean(
            String className,
            ObjectName objectName,
            ObjectName loaderName,
            Object[] params,
            String[] signature
            )
        throws
            ReflectionException,
            InstanceAlreadyExistsException,
            MBeanRegistrationException,
            MBeanException,
            NotCompliantMBeanException,
            InstanceNotFoundException, 
		java.io.IOException,
            java.rmi.RemoteException {
    	
    	throw new MBeanException(new Exception("createMBean method is not supported !!"));
    }


    /**
     * @todo Document this method
     *
     * @param objectName X
     * @exception InstanceNotFoundException X
     * @exception MBeanRegistrationException X
     */
    public void unregisterMBean(
            ObjectName objectName
            )
        throws
	    IOException,
            InstanceNotFoundException,
            MBeanRegistrationException, 
            java.rmi.RemoteException {
    	
    	throw new MBeanRegistrationException(new Exception("unregisterMBean method is not supported !!"));    	
    }


    /**
     * @todo Document: Getter for ObjectInstance attribute of the
     * RemoteMBeanServer object
     *
     * @param objectName X
     * @return X
     * @exception InstanceNotFoundException X
     */
    public ObjectInstance getObjectInstance(
            ObjectName objectName
            )
        throws
	    IOException,
            InstanceNotFoundException, 
            java.rmi.RemoteException {
    	
    	throw new InstanceNotFoundException("getObjectInstance method is not supported !!");    	
    }


    /**
     * @todo Document this method
     *
     * @param objectName X
     * @param pQuery X
     * @return X
     */
    public Set queryMBeans(
            ObjectName objectName,
            QueryExp pQuery
            ) throws IOException,
		java.rmi.RemoteException {
    	
    	throw new IOException("queryMBeans method is not supported !!"); 
    }


    /**
     * @todo Document this method
     *
     * @param objectName X
     * @param pQuery X
     * @return X
     */
    public Set queryNames(
            ObjectName objectName,
            QueryExp pQuery
            ) throws IOException,
		java.rmi.RemoteException {
    	
    	return null;
    	
    }


    /**
     * @todo Document: Getter for Registered attribute of the RemoteMBeanServer
     * object
     *
     * @param objectName X
     * @return X
     */
    public boolean isRegistered(
            ObjectName objectName
            ) throws IOException,
		java.rmi.RemoteException {
    	
    	return false;
    }


    /**
     * @todo Document: Getter for InstanceOf attribute of the RemoteMBeanServer
     * object
     *
     * @param objectName X
     * @param className X
     * @return X
     * @exception InstanceNotFoundException X
     */
    public boolean isInstanceOf(
            ObjectName objectName,
            String className
            )
        throws
	    IOException,
            InstanceNotFoundException, 
            java.rmi.RemoteException {
    	
    	throw new InstanceNotFoundException("isInstanceOf method is not supported !!"); 
    }
    

    /**
     * @todo Document: Getter for MBeanCount attribute of the RemoteMBeanServer
     * object
     * @return X
     */
    public Integer getMBeanCount(
            ) throws IOException,
		java.rmi.RemoteException {
    
    	return null;
    }


    /**
     * @todo Document: Getter for Attribute attribute of the RemoteMBeanServer
     * object
     *
     * @param objectName X
     * @param pAttribute X
     * @return X
     * @exception MBeanException X
     * @exception AttributeNotFoundException X
     * @exception InstanceNotFoundException X
     * @exception ReflectionException X
     */
    public Object getAttribute(
            ObjectName objectName,
            String pAttribute
            )
        throws
	    IOException,
            MBeanException,
            AttributeNotFoundException,
            InstanceNotFoundException,
            ReflectionException, 
            java.rmi.RemoteException {
    	
    	return null;
    }


    /**
     * @todo Document: Getter for Attributes attribute of the RemoteMBeanServer
     * object
     *
     * @param objectName X
     * @param pAttributes X
     * @return X
     * @exception InstanceNotFoundException X
     * @exception ReflectionException X
     */
    public AttributeList getAttributes(
            ObjectName objectName,
            String[] pAttributes
            )
        throws
	    IOException,
            InstanceNotFoundException,
            ReflectionException, 
            java.rmi.RemoteException {
    	
    	return null;
    }


    /**
     * @todo Document: Setter for Attribute attribute of the RemoteMBeanServer
     * object
     *
     * @param objectName attribute
     * @param pAttribute attribute
     * @exception InstanceNotFoundException X
     * @exception AttributeNotFoundException X
     * @exception InvalidAttributeValueException X
     * @exception MBeanException X
     * @exception ReflectionException X
     */
    public void setAttribute(
            ObjectName objectName,
            Attribute pAttribute
            )
        throws
	    IOException,
            InstanceNotFoundException,
            AttributeNotFoundException,
            InvalidAttributeValueException,
            MBeanException,
            ReflectionException, 
            java.rmi.RemoteException {
    	
    }


    /**
     * @todo Document: Setter for Attributes attribute of the RemoteMBeanServer
     * object
     *
     * @param objectName attributes
     * @param pAttributes attributes
     * @return X
     * @exception InstanceNotFoundException X
     * @exception ReflectionException X
     */
    public AttributeList setAttributes(
            ObjectName objectName,
            AttributeList pAttributes
            )
        throws
	    IOException,
            InstanceNotFoundException,
            ReflectionException, 
            java.rmi.RemoteException {
    	
    	return null;
    	
    }


    /**
     * @todo Document this method
     *
     * @param objectName X
     * @param actionName X
     * @param params X
     * @param signature X
     * @return X
     * @exception InstanceNotFoundException X
     * @exception MBeanException X
     * @exception ReflectionException X
     */
    public Object invoke(
            ObjectName objectName,
            String actionName,
            Object[] params,
            String[] signature
            )
        throws
	    IOException,
            InstanceNotFoundException,
            MBeanException,
            ReflectionException, 
            java.rmi.RemoteException {
    	
    	return null;
    	
    }

    

    /**
     * @todo Document: Getter for DefaultDomain attribute of the
     * RemoteMBeanServer object
     * @return X
     */
    public String getDefaultDomain(
            ) throws 
		IOException,
		java.rmi.RemoteException {
    	
    	return null;
    	
    }

    
    public String[] getDomains() throws IOException {
    	
      	throw new IOException("getDomains method is not supported !!"); 
    }

    /**
     * @todo Document: Getter for MBeanInfo attribute of the RemoteMBeanServer
     * object
     *
     * @param objectName X
     * @return X
     * @exception InstanceNotFoundException X
     * @exception IntrospectionException X
     * @exception ReflectionException X
     */
    public MBeanInfo getMBeanInfo(
            ObjectName objectName
            )
        throws
	    IOException,
            InstanceNotFoundException,
            IntrospectionException,
            ReflectionException, 
            java.rmi.RemoteException {
    	
    	return null;
    	
    }


    /**
     * Adds a feature to the NotificationListener attribute of the
     * RemoteMBeanServer object
     *
     * @param objectName The feature to be added to the NotificationListener
     *      attribute
     * @param listener The feature to be added to the NotificationListener
     *      attribute
     * @param filter The feature to be added to the NotificationListener
     *      attribute
     * @param handback The feature to be added to the NotificationListener
     *      attribute
     * @exception InstanceNotFoundException X
     */
    public void addNotificationListener(
            ObjectName objectName,
            NotificationListener listener,
            NotificationFilter filter,
            Object handback
            )
        throws
	    IOException,
            InstanceNotFoundException, 
            java.rmi.RemoteException {
    	
      	throw new InstanceNotFoundException("addNotificationListener method is not supported !!");     	
    }


    /**
     * Adds a feature to the NotificationListener attribute of the
     * RemoteMBeanServer object
     *
     * @param objectName The feature to be added to the NotificationListener
     *      attribute
     * @param listener The feature to be added to the NotificationListener
     *      attribute
     * @param filter The feature to be added to the NotificationListener
     *      attribute
     * @param handback The feature to be added to the NotificationListener
     *      attribute
     * @exception InstanceNotFoundException X
     */
    public void addNotificationListener(
            ObjectName objectName,
            ObjectName listener,
            NotificationFilter filter,
            Object handback
            )
        throws
	    IOException,
            InstanceNotFoundException, 
            java.rmi.RemoteException {
    	
    	throw new InstanceNotFoundException("addNotificationListener method is not supported !!");      	
    }


    

    /**
     * @todo Document this method
     *
     * @param objectName X
     * @param listener X
     * @exception InstanceNotFoundException X
     * @exception ListenerNotFoundException X
     * @exception UnsupportedOperationException X
     */
    public void removeNotificationListener(
            ObjectName objectName,
            ObjectName listener
            )
        throws
	    IOException,
            InstanceNotFoundException,
            ListenerNotFoundException,
            UnsupportedOperationException, 
            java.rmi.RemoteException {
    	
    	throw new InstanceNotFoundException("removeNotificationListener method is not supported !!");      	
    }

    
    /**
     * @todo Document this method
     *
     * @param objectName X
     * @param listener X
     * @exception InstanceNotFoundException X
     * @exception ListenerNotFoundException X
     */
    public void removeNotificationListener(
            ObjectName objectName,
            NotificationListener listener
            )
        throws
	    IOException,
            InstanceNotFoundException,
            ListenerNotFoundException, 
            java.rmi.RemoteException {
    	
    	throw new InstanceNotFoundException("removeNotificationListener method is not supported !!");      	
    }
    
    /**
     * 
     */
    public void removeNotificationListener(
    		ObjectName name,
    		NotificationListener listener, 
			NotificationFilter filter, 
			Object handback) 
    	throws
		    IOException,
	            InstanceNotFoundException,
	            ListenerNotFoundException, 
	            java.rmi.RemoteException {
    	
    	throw new InstanceNotFoundException("removeNotificationListener method is not supported !!");      	
    }
    
    
    /**
     * 
     */
    public void removeNotificationListener(
    		ObjectName name, 
			ObjectName listener, 
			NotificationFilter filter, 
			Object handback) 
    	throws
	    IOException,
	        InstanceNotFoundException,
	        ListenerNotFoundException, 
	        java.rmi.RemoteException {
    	
    	throw new InstanceNotFoundException("removeNotificationListener method is not supported !!");      
    }
    
    public int getHostType() {
        return  mHostType ;

    }
    
}
