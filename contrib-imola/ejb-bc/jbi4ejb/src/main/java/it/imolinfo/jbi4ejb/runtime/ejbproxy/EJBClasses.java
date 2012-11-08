/*******************************************************************************
 *  Copyright (c) 2005, 2006, 2007 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 *******************************************************************************/
package it.imolinfo.jbi4ejb.runtime.ejbproxy;

/**
 * Mantains all the classes information of the Stateless EJB.
 */
public class EJBClasses {
    
    /** The ejb classes path. */
    private String ejbClassesPath;
    
    /** The remote interface class name. */
    private String remoteInterfaceClassName;
    
    /**
     * Instantiates a new EJBClasses.
     * 
     * @param ejbClassesPath
     *             The file systempath to the ejb classes
     * @param remoteInterfaceClassName
     *             The remote interface class name 
     */
    public EJBClasses(String ejbClassesPath, String remoteInterfaceClassName) {
        this.ejbClassesPath = ejbClassesPath;
        this.remoteInterfaceClassName = remoteInterfaceClassName;
    }

    /**
     * Gets the ejb classes path.
     * 
     * @return the ejb classes path
     */
    public String getEjbClassesPath() {
        return ejbClassesPath;
    }

    /**
     * Gets the remote interface class name.
     * 
     * @return the remote interface class name
     */
    public String getRemoteInterfaceClassName() {
        return remoteInterfaceClassName;
    }        

}
