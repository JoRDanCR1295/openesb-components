 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.webservice.generator;

/**
 * This class is used to define a custom code generation.
 */
public class CodeGenerationProperties {

    /**
     * If this property is 'true' (default) the valuetype implementation is
     * generated with the methods toString and equals.
     */
    private boolean valueTypeImplementationWithToStringAndEquals = true;
    
    /**
     * Creates a new instance of CodeGenerationProperties.
     */
    public CodeGenerationProperties() {
        // NOP
    }
    
/**
 * 
 * @return  The reurn
 */
    public boolean isValueTypeImplementationWithToStringAndEquals() {
        return valueTypeImplementationWithToStringAndEquals;
    }

    /**
     * 
     * @param  valueTypeImplementationWithToStringAndEquals  The value type implementation with to string and equals
     */
    public void setValueTypeImplementationWithToStringAndEquals(boolean valueTypeImplementationWithToStringAndEquals) {
        this.valueTypeImplementationWithToStringAndEquals = valueTypeImplementationWithToStringAndEquals;
    }
    
}
