/*******************************************************************************
 *  Copyright (c) 2005, 2006, 2007 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 *******************************************************************************/
package it.imolinfo.jbi4ejb.jbi.component;

import it.imolinfo.jbi4ejb.jbi.component.runtime.ComponentRuntime;

import javax.jbi.component.ComponentLifeCycle;
import javax.jbi.component.ServiceUnitManager;

/**
 * Jbi4EJB Binding Component. 
 * @see Jbi4EjbLifeCycle for more details of the generated code.
 *
 * @author <a href="mailto:mpiraccini@imolinfo.it">Marco Piraccini</a>
 */
public class Jbi4EjbRuntime extends ComponentRuntime {
        
    
    /**
     * constructor.
     */
    public Jbi4EjbRuntime() {
        super();
    }
    
    /**
     * creates the Component specific implementation of the ComponentLifeCycle.
     * 
     * @return the <code>Jbi4EjbLifeCycle</code> instance
     */
    protected ComponentLifeCycle createComponentLifeCycle() {
        return new Jbi4EjbLifeCycle(this);
    }
    
    /**
     * creates the Component specific implementation of the ServiceUnitManager.
     * 
     * @return the <code>Jbi4EjbSUManager</code> instance
     */
    protected ServiceUnitManager createServiceUnitManager() {
        return new Jbi4EjbSUManager(this);
    }
    
}
