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
 * @(#)BasicNodeSet.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.apache.commons.jxpath;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * A simple implementation of NodeSet that behaves as a collection of pointers. 
 * @author Dmitri Plotnikov
 * @version  
 */
public class BasicNodeSet implements NodeSet {
    private List pointers = new ArrayList();
    private List readOnlyPointers;
    private List nodes;
    private List values;

    public void add(Pointer pointer) {
        pointers.add(pointer);
        readOnlyPointers = null;
    }
    
    public void remove(Pointer pointer) {
        pointers.remove(pointer);
        readOnlyPointers = null;
    }
    
    public List getPointers() {
        if (readOnlyPointers == null) {
            readOnlyPointers = Collections.unmodifiableList(pointers);
        }
        return readOnlyPointers;
    }

    public List getNodes() {
        if (nodes == null) {
            nodes = new ArrayList();
            for (int i = 0; i < pointers.size(); i++) {
                Pointer pointer = (Pointer) pointers.get(i);
                nodes.add(pointer.getValue());
            }
            nodes = Collections.unmodifiableList(nodes);
        }
        return nodes;
    }

    public List getValues() {
        if (values == null) {
            values = new ArrayList();
            for (int i = 0; i < pointers.size(); i++) {
                Pointer pointer = (Pointer) pointers.get(i);
                values.add(pointer.getValue());
            }
            values = Collections.unmodifiableList(values);
        }
        return values;
    }
    
    public String toString() {
        return pointers.toString();
    }
}
