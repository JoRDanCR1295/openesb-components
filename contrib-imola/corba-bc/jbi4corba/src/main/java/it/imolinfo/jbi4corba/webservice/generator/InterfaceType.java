 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.webservice.generator;


/**
 * 
 * class InterfaceType - define union type class 
 * @author lacquaviva
 */


public  class InterfaceType implements SearchedType{

    private String typeName;
     
    /**
     * Constructor
     */
    public InterfaceType() {
      
    }

    /**
     * Constructor
     * @param name - name of the Interface
     * 
     */
    public InterfaceType(String name) {
        typeName = name;
        
    }

    /**
     * 
     * @return typeName
     */
    public String getTypeName() {
        return typeName;
    }

    /**
     * 
     * @param typeName
     */
    public void setTypeName(String typeName) {
        this.typeName = typeName;
    }
    

       
}