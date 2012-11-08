/*
 * @(#)BusinessObjectsNotFoundException.java        $Revision: 1.1 $ $Date: 2008/12/17 23:24:43 $
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

package org.openesb.components.rules4jbi.shared.classloader;

/**
 * Thrown by all variations of the <code>createServiceUnitClassLoader</code> method
 * in the <code>ClassLoaderFactory</code> class, when user did not provide a valid
 * source of business objects. A valid source of business objects can be either
 * a not-empty classes directory, or a (business objects) library directory
 * that contains at least one <code>.jar</code> file.
 * 
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.1 $ $Date: 2008/12/17 23:24:43 $
 * 
 * @since 0.4
 */
public class BusinessObjectsNotFoundException extends RuntimeException {

    private static final long serialVersionUID = -2449366228226639442L;

    public BusinessObjectsNotFoundException(String message) {
        super(message);
    }
}
