 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.jbi.cxf;

import org.apache.cxf.interceptor.WrappedOutInterceptor;

/**
 * This class is a CXF class extended just to correcly add the Wrapper
 * management in the interceptor chain BEFORE the Jbi4CorbaBareOutInterceptor.
 * @author mpiraccini@imolinfo.it
 */
public class Jbi4CorbaWrappedOutInterceptor extends WrappedOutInterceptor  {
    
    public Jbi4CorbaWrappedOutInterceptor() {        
        super();
        addBefore(Jbi4CorbaBareOutInterceptor.class.getName());
    }
    public Jbi4CorbaWrappedOutInterceptor(String phase) {
        super(phase);
        addBefore(Jbi4CorbaBareOutInterceptor.class.getName());
    }
}
