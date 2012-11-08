/*
 * @(#)Direction.java        $Revision: 1.1.1.1 $ $Date: 2008/06/30 08:53:19 $
 * 
 * Copyright (c) 2008 Milan Fort (http://www.milanfort.com/). All rights reserved.
 * 
 * The contents of this file are subject to the terms of the Common Development
 * and Distribution License (the "License"). You may not use this file except
 * in compliance with the License.
 * 
 * You can obtain a copy of the license at http://www.sun.com/cddl/cddl.html.
 * See the License for the specific language governing permissions and limitations
 * under the License.
 */

package org.openesb.components.rules4jbi.netbeans.wsdl.wizard.table;

/**
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.1.1.1 $ $Date: 2008/06/30 08:53:19 $
 * 
 * @since 0.1
 */
public class Direction {
    
    static final String INPUT_STRING = "Input";
    
    static final String OUTPUT_STRING = "Output";
    
    static final String INPUT_VALUE = "IN";
    
    static final String OUTPUT_VALUE = "OUT";
    
    private static final String INITIAL_VALUE = INPUT_VALUE;
    
    private final String value;

    public Direction() {
        this(INITIAL_VALUE);
    }
    
    public Direction(String value) {
        if (INPUT_VALUE.equals(value) || OUTPUT_VALUE.equals(value)) {
            this.value = value;    
            
        } else {
            throw new IllegalArgumentException("Invalid direction value: " + value);
        }
    }

    public boolean isInput() {
        return INPUT_VALUE.equals(value);
    }
    
    public boolean isOutput() {
        return !isInput();
    }
    
    @Override
    public String toString() {
        return value;
    }
    
    public String toDescriptiveString() {
        return isInput() ? INPUT_STRING : OUTPUT_STRING;
    }
}
