/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)ConnectionConfiguration.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.bpel.core.bpel.connection;

import java.util.Properties;

import javax.naming.InitialContext;

/**
 * Interface for connection configuration requirements that the BPEL engine requires. 
 * The context in which the BPEL engine resides should register the implementation class 
 * on the Engine as part of its initialization. The engine will rely on the implementation
 * to create the necessary connections and resources.
 * The intention is to keep the implementation details of such configurations independent of 
 * the core engine, wherby isolating the core engine from any dependency on the context in 
 * which it resides/run's in.
 * 
 * @author Sun Microsystems.
 *
 */
public interface ConnectionConfiguration extends ConnectionProperties {
	
    /**
     * Create the connection pools and jdbc jndi resources that are needed for the engine 
     * persistence by invoking the ConnectionConfiguration. This is done only if
     * Persistence is enabled for the BPEL engine. 
     * BPEL engine is running in non clustered mode.
     * ConnectionConfiguration Implementation is set on the engine. Note that In the case 
     * of junit test cases this class is not set.
     * 
     * NOTE: This creation assumes that the Derby DB is started, in other words
     * a Derby DB that is not started or not present will lead to a Exception that is not
     * propagated to the JBI Framework. Such a failure will be logged at the SEVERE level.
     * Currently this is not supported for any other DB.
	 * Called by the core engine to create the connection pools and jdbc resources that are 
	 * needed for its persistence functionality. 
	 */
	public void createConnectionsPools();
	
	/**
	 * Access to the new naming context once the ConnectionConfiguration has updated the 
	 * InitialContext, like adding new JNDI names, etc.
	 * @return an instance of the InitialContext.
	 */
	public InitialContext getNamingContext();
    
    /**
     * Returns connection properties
     * @return
     */
    public Properties getConnectionProperties ();

}
