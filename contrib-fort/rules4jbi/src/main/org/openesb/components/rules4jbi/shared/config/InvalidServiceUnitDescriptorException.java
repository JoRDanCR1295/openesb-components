/*
 * @(#)InvalidServiceUnitDescriptorException.java        $Revision: 1.2 $ $Date: 2008/07/03 05:46:01 $
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

package org.openesb.components.rules4jbi.shared.config;

/**
 * Thrown by <code>load</code> method in the <code>ServiceUnitDescriptor</code> class,
 * when it cannot load the service unit descriptor.
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.2 $ $Date: 2008/07/03 05:46:01 $
 * 
 * @since 0.1
 */
public class InvalidServiceUnitDescriptorException extends Exception {
    
    private static final long serialVersionUID = -2449366228226639442L;
    
    public InvalidServiceUnitDescriptorException(String message) {
        super(message);
    }
    
    public InvalidServiceUnitDescriptorException(String message, Exception cause) {
        super(message, cause);
    }
    
    public InvalidServiceUnitDescriptorException(Exception cause) {
        super(cause);
    }
}
