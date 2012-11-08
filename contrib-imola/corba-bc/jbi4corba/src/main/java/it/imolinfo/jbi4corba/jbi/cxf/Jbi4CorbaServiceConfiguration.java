 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.jbi.cxf;

import org.apache.cxf.service.factory.DefaultServiceConfiguration;
import org.apache.cxf.service.model.MessagePartInfo;

/**
 * CXF Service Configuration that ovverides the minOccurss calculation for the wrapper
 * types. With this calss we can force not-null message part (is not possible 
 * to force that using jax-ws 2.1 - should be possible woth jax-ws 2.2). See
 * http://www.nabble.com/CXF---JAXB-%22Java-first-approach%22---remove-minOccurs%3D%220%22-td21692084.html#a22052705
 * @author marco
 *
 */
public class Jbi4CorbaServiceConfiguration extends DefaultServiceConfiguration {
	
	/**
	 * Returns always "1".
	 */
    public Long getWrapperPartMinOccurs(MessagePartInfo mpi) {            
        return new Long(1);
    }
    
	/**
	 * Returns always "1".
	 */    
    public Long getWrapperPartMaxOccurs(MessagePartInfo mpi) {        
        return new Long(1);
    }    


}
