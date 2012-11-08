/*
 * @(#)Cardinality.java        $Revision: 1.1.1.1 $ $Date: 2008/06/30 08:53:19 $
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
public class Cardinality {
    
    static final String UNBOUNDED_STRING = "Unbounded";
    
    private static final String UNBOUNDED_SYMBOL = "*";
    
    private static final String INITIAL_VALUE = "1";
    
    private final String value;

    private final boolean unbounded;
    
    private final int intValue;
    
    public Cardinality() {
        this(INITIAL_VALUE);
    }
    
    public Cardinality(String value) {
        if (!isValidCardinalityValue(value)) {
            throw new IllegalArgumentException("Invalid cardinality value: " +  value);
        }
        
        this.value = value;
        
        if (UNBOUNDED_SYMBOL.equals(value)) {
            unbounded = true;
            intValue = Integer.MAX_VALUE;
            
        } else {
            unbounded = false;
            
            intValue = Integer.parseInt(value);
        }
    }

    public static boolean isValidCardinalityValue(String value) {
        if (value == null) {
            return false;
        }

        if (value.trim().equals("")) {
            return false;
        }
        
        if (value.equals(UNBOUNDED_SYMBOL)) {
            return true;
        }
        
        int intValue = -1;
        try {
            intValue = Integer.parseInt(value);

        } catch (NumberFormatException e) {
            return false;
        }

        if (intValue <= 0 || intValue == Integer.MAX_VALUE) {
            return false;
        }

        return true;
    }
    
    public int intValue() {
        return intValue;
    }
    
    public boolean isUnbounded() {
        return unbounded;
    }
    
    @Override
    public String toString() {
        return value;
    }

    public String toDescriptiveString() {
        return isUnbounded() ? UNBOUNDED_STRING : value;
    }
}
