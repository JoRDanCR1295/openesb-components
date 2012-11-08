/*
 * @(#)QNameComparator.java        $Revision: 1.1 $ $Date: 2008/07/25 06:08:40 $
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

package org.openesb.components.rules4jbi.engine.component;

import java.util.Comparator;
import java.util.List;

import javax.xml.namespace.QName;

import org.openesb.components.rules4jbi.shared.util.JAXBUtils;

/**
 * A <code>Comparator</code> that can be used to ensure ordering of objects
 * by their XML element qualified names according to a specified ordered list
 * of <code>QName</code>s.
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.1 $ $Date: 2008/07/25 06:08:40 $
 * 
 * @see java.util.Comparator
 * @since 0.1
 */
public final class QNameComparator implements Comparator<Object> {

    private final List<QName> orderedObjectNames;

    public QNameComparator(List<QName> orderedObjectNames) {
        this.orderedObjectNames = orderedObjectNames;
    }
    
    public int compare(Object obj1, Object obj2) {
        return orderedObjectNames.indexOf(JAXBUtils.getElementName(obj1))
                - orderedObjectNames.indexOf(JAXBUtils.getElementName(obj2));
    }
}
