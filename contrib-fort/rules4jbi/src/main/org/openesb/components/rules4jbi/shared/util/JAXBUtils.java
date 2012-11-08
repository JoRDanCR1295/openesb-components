/*
 * @(#)JAXBUtils.java        $Revision: 1.1 $ $Date: 2008/07/25 04:55:17 $
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

package org.openesb.components.rules4jbi.shared.util;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.JAXBIntrospector;
import javax.xml.namespace.QName;

/**
 * This class provides various JAXB specific utilities.
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.1 $ $Date: 2008/07/25 04:55:17 $
 * 
 * @since 0.1
 */
public final class JAXBUtils {
    
    private JAXBUtils() {}
    
    public static QName getElementName(Object obj) {
        try {
            JAXBContext context = JAXBContext.newInstance(obj.getClass());
            JAXBIntrospector introspector = context.createJAXBIntrospector();
            
            return introspector.getElementName(obj);

        } catch (JAXBException e) {
            return null;
        }
    }
}
