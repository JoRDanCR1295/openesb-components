/* *************************************************************************
 *
 *          Copyright (c) 2005, Sun Microsystems,
 *          All Rights Reserved
 *
 *          This program, and all the routines referenced herein,
 *          are the proprietary properties and trade secrets of
 *          Sun Microsystems.
 *
 *          Except as provided for by license agreement, this
 *          program shall not be duplicated, used, or disclosed
 *          without  written consent signed by an officer of
 *          Sun Microsystems.
 *
 ***************************************************************************/

package com.sun.jbi.dcombc.extensions;

import java.util.Map;

import javax.wsdl.extensions.ExtensionRegistry;

/**
 * DCOMExtensionRegistry is registry to register the wsdl extensions
 * 
 * @author Chandrakanth Belde
 */
public class DCOMExtensionRegistry extends ExtensionRegistry {
	/**
	 *
	 */
    private static final long serialVersionUID = 1L;

    /** 
	 * Creates a new instance of DCOMExtensionRegistry 
	 */
    public DCOMExtensionRegistry() {
        super();
        DCOMExtSerializer dcomExtSerializer = new DCOMExtSerializer();
        dcomExtSerializer.registerSerializer(this);
    }

    /** Creates a new instance of DCOMExtensionRegistry */
    public DCOMExtensionRegistry(Map envVarConfigMap) {
        super();
        DCOMExtSerializer dcomExtSerializer = new DCOMExtSerializer(envVarConfigMap);
        dcomExtSerializer.registerSerializer(this);
    }  
}
