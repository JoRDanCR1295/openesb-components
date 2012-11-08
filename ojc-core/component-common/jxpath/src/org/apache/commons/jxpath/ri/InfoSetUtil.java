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
 * @(#)InfoSetUtil.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.apache.commons.jxpath.ri;

import org.apache.commons.jxpath.Pointer;
import org.apache.commons.jxpath.ri.model.NodePointer;
import org.apache.commons.jxpath.ri.model.VariablePointer;
import org.w3c.dom.Node;

/**
 * Type conversions, XPath style.
 *
 * @author Dmitri Plotnikov
 * @version  
 */
public class InfoSetUtil {

    private static final Double ZERO = new Double(0);
    private static final Double ONE = new Double(1);
    private static final Double NOT_A_NUMBER = new Double(Double.NaN);


    /**
     * Converts the supplied object to String
     */
    public static String stringValue(Object object) {
        if (object instanceof String) {
            return (String) object;
        }
        else if (object instanceof Number) {
            double d = ((Number) object).doubleValue();
            long l = ((Number) object).longValue();
            if (d == l) {
                return String.valueOf(l);
            }
            return String.valueOf(d);
        }
        else if (object instanceof Boolean) {
            return ((Boolean) object).booleanValue() ? "true" : "false";
        }
        else if (object == null) {
            return "";
        }
        else if (object instanceof NodePointer) {
            return stringValue(((NodePointer) object).getValue());
        }
        else if (object instanceof EvalContext) {
            EvalContext ctx = (EvalContext) object;
            Pointer ptr = ctx.getSingleNodePointer();
            if (ptr != null) {
                return stringValue(ptr);
            }
            return "";
        }
        return String.valueOf(object);
    }

    /**
     * Converts the supplied object to Number
     */
    public static Number number(Object object) {
        if (object instanceof Number) {
            return (Number) object;
        }
        else if (object instanceof Boolean) {
            return ((Boolean) object).booleanValue() ? ONE : ZERO;
        }
        else if (object instanceof String) {
            Double value;
            try {
                value = new Double((String) object);
            }
            catch (NumberFormatException ex) {
                value = NOT_A_NUMBER;
            }
            return value;
        }
        else if (object instanceof EvalContext) {
            EvalContext ctx = (EvalContext) object;
            Pointer ptr = ctx.getSingleNodePointer();
            if (ptr != null) {
                return number(ptr);
            }
            return NOT_A_NUMBER;
        }
        else if (object instanceof NodePointer) {
            return number(((NodePointer) object).getValue());
        }
        return number(stringValue(object));
    }

    /**
     * Converts the supplied object to double
     */
    public static double doubleValue(Object object) {
        if (object instanceof Number) {
            return ((Number) object).doubleValue();
        }
        else if (object instanceof Boolean) {
            return ((Boolean) object).booleanValue() ? 0.0 : 1.0;
        }
        else if (object instanceof String) {
            if (object.equals("")) {
                return 0.0;
            }

            double value;
            try {
                value = Double.parseDouble((String) object);
            }
            catch (NumberFormatException ex) {
                value = Double.NaN;
            }
            return value;
        }
        else if (object instanceof NodePointer) {
            return doubleValue(((NodePointer) object).getValue());
        }
        else if (object instanceof EvalContext) {
            EvalContext ctx = (EvalContext) object;
            Pointer ptr = ctx.getSingleNodePointer();
            if (ptr != null) {
                return doubleValue(ptr);
            }
            return Double.NaN;
        }
        return doubleValue(stringValue(object));
    }

    /**
     * Converts the supplied object to boolean
     */
    public static boolean booleanValue(Object object) {
        if (object instanceof Number) {
            double value = ((Number) object).doubleValue();
            return value != 0 && value != -0 && !Double.isNaN(value);
        }
        else if (object instanceof Boolean) {
            return ((Boolean) object).booleanValue();
        }
        else if (object instanceof EvalContext) {
            EvalContext ctx = (EvalContext) object;
            Pointer ptr = ctx.getSingleNodePointer();
            if (ptr == null) {
                return false;
            }
            
            return booleanValue(ptr);
        }
        else if (object instanceof String) {
            return ((String) object).length() != 0;
        }
        else if (object instanceof NodePointer) {
            NodePointer pointer = (NodePointer) object;

            if (((NodePointer) object).getValue() instanceof Boolean) {
                Boolean booleanValue = (Boolean)((NodePointer) object).getValue();
                return booleanValue.booleanValue();
            }
            
            if (pointer instanceof VariablePointer) {
                return booleanValue(pointer.getNode());
            }

            pointer = pointer.getValuePointer();
            return pointer.isActual();
        }
        
        else if (object == null) {
            return false;
        }
        return true;
    }
}
