/*
 * @(#)file      RFC1213_MIB_IMPL.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.26
 * @(#)lastedit  04/04/07
 *
 * Copyright 2004 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package com.sun.soabi.snmpagent;

// jmx imports
//
import javax.management.MBeanServer;
import javax.management.ObjectName;

import com.sun.management.snmp.SnmpStatusException;
import com.sun.management.snmp.agent.SnmpMibRequest;

/**
 * This class shows how a generated MIB class can be subclassed to
 * provide an implementation of the MIB.
 * <p>In particular, it shows how to override MBean factory methods
 *    in order to substitute the generated MBean skeletons with
 *    a specific implementation.</p>
 *
 * <p>The recommended steps to follow when developing a MIB are thus:
 * <ul>
 *     <li>For each group you want to provide an implementation of, 
 *         create a subclass of this group in which you will fill
 *         the get/set methods you need to override. (e.g.: for the "snmp"
 *         group of MIB-II, provide an SnmpImpl class extending Snmp
 *         in which the get/set method will be implemented).
 *     <li>Subclass the generated MIB (if not yet done), redefine
 *         the factory method for the group you want to implement, and
 *         make it return an instance of your implementation class in place
 *         of the generated "dummy" skeleton (e.g.: override the 
 *         <code>createSnmpMBean()</code> factory method and make it
 *         return an instance of SnmpImpl instead of an instance of Snmp.
 * </ul>
 * <p>By proceeding this way you should never have to modify directly
 *    the generated files, which will greatly ease the development 
 *    process. </p>
 *
 **/
class RFC1213_MIB_IMPL extends RFC1213_MIB {

    public RFC1213_MIB_IMPL() {
	super();
    }
    /**
     * Passing it a name in order to register the same mib in 2 MBeanServer.
     */
    public RFC1213_MIB_IMPL(String name) {
	super();
	mibName = name;
    }
    /**
     * Factory method for "Snmp" group MBean.
     * 
     * This method is redefined in order to replace the instance of the 
     * generated Snmp class with an instance of a customized class 
     * (SnmpImpl) implementing the Snmp group. 
     * This class is provided for the sake of example and shouldn't be
     * considered as a "real-world" implementation. 
     * 
     * @param groupName Name of the group ("Snmp")
     * @param groupOid  OID of this group
     * @param groupObjname ObjectName for this group (may be null)
     * @param server    MBeanServer for this group (may be null)
     * 
     * @return An instance of the MBean class implementing the
     *         "Snmp" group (SnmpImpl)
     * 
     * Note that when using standard metadata,
     * the returned object must implement the "InterfacesMBean"
     * interface.
     **/
    protected Object createSnmpMBean(String groupName, String groupOid, 
				     ObjectName groupObjname, 
				     MBeanServer server)  {

        // Note that when using standard metadata,
        // the returned object must implement the "InterfacesMBean"
        // interface.
        //
        if (server != null) 
            return new SnmpImpl(this,server);
        else 
            return new SnmpImpl(this);
    }

    /**
     * Factory method for "System" group MBean.
     * 
     * This method is redefined in order to replace the instance of the 
     * generated System class with an instance of a customized class 
     * (SystemImpl) implementing the System group. 
     * This class is provided for the sake of example and shouldn't be
     * considered as a "real-world" implementation. 
     * 
     * @param groupName Name of the group ("System")
     * @param groupOid  OID of this group
     * @param groupObjname ObjectName for this group (may be null)
     * @param server    MBeanServer for this group (may be null)
     * 
     * @return An instance of the MBean class implementing the
     *         "System" group (SystemImpl)
     * 
     * Note that when using standard metadata,
     * the returned object must implement the "InterfacesMBean"
     * interface.
     **/
    protected Object createSystemMBean(String groupName, 
				       String groupOid, 
				       ObjectName groupObjname, 
				       MBeanServer server)  {

        // Note that when using standard metadata,
        // the returned object must implement the "InterfacesMBean"
        // interface.
        //
        if (server != null) 
            return new SystemImpl(this,server);
        else 
            return new SystemImpl(this);
    }

    /**
     * Factory method for "Interfaces" group MBean.
     * 
     * This method is redefined in order to replace the instance of the 
     * generated Interfaces class with an instance of a customized class 
     * (InterfacesImpl) implementing the Interfaces group. 
     * This class is provided for the sake of example and shouldn't be
     * considered as a "real-world" implementation. 
     * 
     * @param groupName Name of the group ("Interfaces")
     * @param groupOid  OID of this group
     * @param groupObjname ObjectName for this group (may be null)
     * @param server    MBeanServer for this group (may be null)
     * 
     * @return An instance of the MBean class generated for the
     *         "Interfaces" group (InterfacesImpl)
     * 
     * Note that when using standard metadata,
     * the returned object must implement the "InterfacesMBean"
     * interface.
     **/
    protected Object createInterfacesMBean(String groupName, 
					   String groupOid, 
					   ObjectName groupObjname, 
					   MBeanServer server)  {

        // Note that when using standard metadata,
        // the returned object must implement the "InterfacesMBean"
        // interface.
        //
        if (server != null) 
            return new InterfacesImpl(this,server);
        else 
            return new InterfacesImpl(this);
    }
    
}
