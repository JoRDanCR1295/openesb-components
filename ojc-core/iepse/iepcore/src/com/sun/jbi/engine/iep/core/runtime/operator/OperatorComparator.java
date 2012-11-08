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
 * @(#)OperatorComparator.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.iep.core.runtime.operator;

import java.io.Serializable;
import java.util.Comparator;

/*
 * OperatorComparator.java
 *
 * Created on July 26, 2005, 4:07 PM
 *
 * @author Bing Lu
 */
public class OperatorComparator implements Comparator<Operator>, Serializable {
    private static OperatorComparator theOne; 
    
    private OperatorComparator() {
    }
    
    private static synchronized void createInstance() {
        if (theOne == null) {
            theOne = new OperatorComparator();
        }
    }

    public static OperatorComparator getInstance() {
        if (theOne == null) {
            createInstance();
        }
        return theOne;
    }

    @Override
    public boolean equals(Object o) {
        return this == o;
    }
    
    @Override
    public int hashCode() {
        assert false : "hashCode not designed";
        return 42; // any arbitrary constant will do 
    }    

    public int compare(Operator op1, Operator op2) {
        return op1.getTopoScore() - op2.getTopoScore();
    }
    
}
