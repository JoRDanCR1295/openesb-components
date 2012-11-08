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



import java.io.IOException;
import java.util.Properties;
import java.util.Set;
import java.util.logging.Logger;

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
import javax.management.ReflectionException;
import javax.management.remote.JMXConnectorFactory;
import javax.management.remote.JMXServiceURL;
import javax.management.remote.JMXConnector;


/**
 * JmxConnnector is the client-side component that allows client code to contact a remote MBeanServer.
 * A JMXConnector handles the details of registering notification listeners and receiving notifications 
 * from the remote MBeanServer, as well as providing a way to authenticate to the JMXConnectorServer, and
 * eventually execute operations on behalf of a given javax.security.auth.Subject. 
 * Finally the JMXConnector allows client code to obtain an implementation of the javax.management.MBeanServerConnection 
 * interface that allows to interact with the remote MBeanServer as if it is local. 
 *
 * The preferred way to create a JMXConnector is to use the javax.management.remote.JMXConnectorFactory class: 
 * 
 * @author ylee
 *
 * TODO To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public class JmxConnector extends Connector {
	
    private static final Logger logger = Logger.getLogger(JmxConnector.class.getName());
    JMXConnector mJMXConnector;
    
    /**
     * Allows connection to remote MBean server via JSR160 protocol
     * @param protocol - 
     * @param host - name of the host for remote host where the MBeanServer is residing
     * @param port - port of the host for remote host where the MBeanServer is residing
     * @param username - 
     * @param password - 
     */
    public JmxConnector(String protocol, String hostname, String port, String username, 
                    String password,int hostType) {
    														// JNDI name
    	super(protocol,hostname,port,username,password,null,hostType);
    }

    public JmxConnector(String protocol, String hostname, String port, String username, String password, 
                    String jndiName, int hostType) {
        // JNDI name
    	super(protocol,hostname,port,username,password,jndiName,hostType);
    }
    
    
    /**
     *  This method returns the MBeanServerConnection to used to invoke the
     *  remote MBean methods
     *  @return MBeanServerConnection
     */
    public AbstractConnector getConnector() throws Exception {
    	Properties env = new Properties();
    	// properties contain user credentials, protocol, authentication, etc
    	// default is <null> properties
    	return getConnector(env);
    }   	
    
    
    /**
     *  This method returns the MBeanServerConnection which can be used to invoke the
     *  remote MBean methods and register for notifications
     *  @return MBeanServerConnection
     */
    public AbstractConnector getConnector(Properties env) throws Exception {
        try {
            JMXServiceURL serviceURL;
            if ( jndiName!=null ) {
            	serviceURL = new JMXServiceURL(protocol, hostname, Integer.parseInt(port),jndiName);
            } else {
                serviceURL = new JMXServiceURL(protocol, hostname, Integer.parseInt(port));
            }
            JMXConnector connector = JMXConnectorFactory.connect(serviceURL, (java.util.Hashtable)env);
            mbeanServerConnector = connector.getMBeanServerConnection();
            return this;
        } catch (Exception exception) {
            throw exception;
        }
        
    }   	
    
    
    // Public --------------------------------------------------------

    /**
     * @todo Document this method
     *
     * @param className X
     * @param name X
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
    	
    	return mbeanServerConnector.createMBean(className,objectName);
    }


    /**
     * @todo Document this method
     *
     * @param className X
     * @param name X
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
    	
    	return mbeanServerConnector.createMBean(className,objectName,loaderName);
    }


    /**
     * Instantiates the given class and registers it on the remote MBeanServer
     * and returns an Object Instance of the MBean.
     *
     * @param className Class name of the class to be loaded and instantiated
     * @param nameToAssign Object Name the new MBean should be assigned to
     * @param params Array of parameter passed to the creator of the class. If
     *      one is of data type Object handler it will be replaced on the
     *      server-side by its effective object.
     * @param signatures Array of Class Names (full qualified) to find the right
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
            ObjectName nameToAssign,
            Object[] params,
            String[] signatures
            )
        throws
            ReflectionException,
            InstanceAlreadyExistsException,
            MBeanRegistrationException,
            MBeanException,
            NotCompliantMBeanException, 
		java.io.IOException,
            java.rmi.RemoteException {
    	
    	return mbeanServerConnector.createMBean(className,nameToAssign,params,signatures);
    }


    /**
     * @todo Document this method
     *
     * @param className X
     * @param objectName X
     * @param loaderName X
     * @param params X
     * @param signatures X
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
            String[] signatures
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
    	
    	return mbeanServerConnector.createMBean(className,objectName,loaderName,params,signatures);
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

    	mbeanServerConnector.unregisterMBean(objectName);
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
    	
    	return mbeanServerConnector.getObjectInstance(objectName);
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
            QueryExp query
            ) throws IOException,
		java.rmi.RemoteException {
    	
    	return mbeanServerConnector.queryMBeans(objectName,query);
    	
    }


    /**
     * @todo Document this method
     *
     * @param objectName X
     * @param query X
     * @return X
     */
    public Set queryNames(
            ObjectName objectName,
            QueryExp query
            ) throws IOException,
		java.rmi.RemoteException {
    	
    	return mbeanServerConnector.queryNames(objectName,query);
    	
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
    	
    	return mbeanServerConnector.isRegistered(objectName);
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
    	
    	return mbeanServerConnector.isInstanceOf(objectName,className);
    }
    

    /**
     * @todo Document: Getter for MBeanCount attribute of the RemoteMBeanServer
     * object
     * @return X
     */
    public Integer getMBeanCount(
            ) throws IOException,
		java.rmi.RemoteException {
    
    	return mbeanServerConnector.getMBeanCount();
    }


    /**
     * @todo Document: Getter for Attribute attribute of the RemoteMBeanServer
     * object
     *
     * @param objectName X
     * @param attribute X
     * @return X
     * @exception MBeanException X
     * @exception AttributeNotFoundException X
     * @exception InstanceNotFoundException X
     * @exception ReflectionException X
     */
    public Object getAttribute(
            ObjectName objectName,
            String attribute
            )
        throws
	    IOException,
            MBeanException,
            AttributeNotFoundException,
            InstanceNotFoundException,
            ReflectionException, 
            java.rmi.RemoteException {
    	
    	return mbeanServerConnector.getAttribute(objectName,attribute);
    }


    /**
     * @todo Document: Getter for Attributes attribute of the RemoteMBeanServer
     * object
     *
     * @param objectName X
     * @param attributes X
     * @return X
     * @exception InstanceNotFoundException X
     * @exception ReflectionException X
     */
    public AttributeList getAttributes(
            ObjectName objectName,
            String[] attributes
            )
        throws
	    IOException,
            InstanceNotFoundException,
            ReflectionException, 
            java.rmi.RemoteException {
    	
    	return mbeanServerConnector.getAttributes(objectName,attributes);
    }


    /**
     * @todo Document: Setter for Attribute attribute of the RemoteMBeanServer
     * object
     *
     * @param objectName attribute
     * @param attribute attribute
     * @exception InstanceNotFoundException X
     * @exception AttributeNotFoundException X
     * @exception InvalidAttributeValueException X
     * @exception MBeanException X
     * @exception ReflectionException X
     */
    public void setAttribute(
            ObjectName objectName,
            Attribute attribute
            )
        throws
	    IOException,
            InstanceNotFoundException,
            AttributeNotFoundException,
            InvalidAttributeValueException,
            MBeanException,
            ReflectionException, 
            java.rmi.RemoteException {
    	
    	mbeanServerConnector.setAttribute(objectName,attribute);    	
    }


    /**
     * @todo Document: Setter for Attributes attribute of the RemoteMBeanServer
     * object
     *
     * @param objectName attributes
     * @param attributes attributes
     * @return X
     * @exception InstanceNotFoundException X
     * @exception ReflectionException X
     */
    public AttributeList setAttributes(
            ObjectName objectName,
            AttributeList attributes
            )
        throws
	    IOException,
            InstanceNotFoundException,
            ReflectionException, 
            java.rmi.RemoteException {
    	
    	return mbeanServerConnector.setAttributes(objectName,attributes);    	
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
    	
    	return mbeanServerConnector.invoke(objectName,actionName,params,signature);    	
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
    	
    	return mbeanServerConnector.getDefaultDomain(); 
    }

    
    public String[] getDomains() throws IOException {
    	
    	return mbeanServerConnector.getDomains(); 
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
    	
    	return mbeanServerConnector.getMBeanInfo(objectName); 
    }


    /**
     * Adds a feature to the NotificationListener attribute of the
     * RemoteMBeanServer object
     *
     * @param objectName The feature to be added to the NotificationListener
     *      attribute
     * @param pListener The feature to be added to the NotificationListener
     *      attribute
     * @param pFilter The feature to be added to the NotificationListener
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
    	
    	mbeanServerConnector.addNotificationListener(objectName,listener,filter,handback); 
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

    	mbeanServerConnector.addNotificationListener(objectName,listener,filter,handback); 
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
    	
    	mbeanServerConnector.removeNotificationListener(objectName,listener); 
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
    	
    	mbeanServerConnector.removeNotificationListener(objectName,listener);     	
    }
    
    /**
     * 
     */
    public void removeNotificationListener(
    		ObjectName objectName,
    		NotificationListener listener, 
			NotificationFilter filter, 
			Object handback)
    		throws 
			IOException,
	            InstanceNotFoundException,
	            ListenerNotFoundException,
	            java.rmi.RemoteException { 
    	
    	mbeanServerConnector.removeNotificationListener(objectName,listener,filter,handback);     	
    }
    
    
    /**
     * 
     */
    public void removeNotificationListener(
    		ObjectName objectName, 
			ObjectName listener, 
			NotificationFilter filter, 
			Object handback) 
    		throws 
			IOException,
	            InstanceNotFoundException,
	            ListenerNotFoundException,
	            java.rmi.RemoteException {

    	mbeanServerConnector.removeNotificationListener(objectName,listener,filter,handback);     	
    }
    
    public JMXConnector getJMXConnector() {
        return mJMXConnector;
    }

}
