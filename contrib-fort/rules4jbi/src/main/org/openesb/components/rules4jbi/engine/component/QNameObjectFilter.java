/*
 * @(#)QNameObjectFilter.java        $Revision: 1.1 $ $Date: 2008/07/25 04:55:16 $
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

import java.util.List;

import javax.rules.ObjectFilter;
import javax.xml.namespace.QName;

import org.openesb.components.rules4jbi.shared.util.JAXBUtils;

/**
 * <code>ObjectFilter</code> that allows only objects with predefined
 * XML element qualified names to pass through.
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.1 $ $Date: 2008/07/25 04:55:16 $
 * 
 * @see javax.rules.ObjectFilter
 * @since 0.1
 */
public final class QNameObjectFilter implements ObjectFilter {
    
    private static final long serialVersionUID = -2449366228226639442L;

    private final List<QName> allowedObjectNames;

    public QNameObjectFilter(List<QName> allowedObjectNames) {
        this.allowedObjectNames = allowedObjectNames;
    }

    public Object filter(Object obj) {
        final QName objectName = JAXBUtils.getElementName(obj);
        
        for (QName allowedObjectName : allowedObjectNames) {
            if (allowedObjectName.equals(objectName)) {
                return obj;
            }
        }
        
        return null;
    }

    public void reset() {
        // do nothing; this filter is stateless
    }
}
