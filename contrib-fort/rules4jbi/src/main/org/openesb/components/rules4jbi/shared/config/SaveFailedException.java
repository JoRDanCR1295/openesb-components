/*
 * @(#)SaveFailedException.java        $Revision: 1.1.1.1 $ $Date: 2008/06/30 08:53:18 $
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
 * This exception is thrown by a <code>Saveable</code> object to indicate
 * that the <code>save()</code> operation failed.
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.1.1.1 $ $Date: 2008/06/30 08:53:18 $
 * 
 * @since 0.1
 */
public class SaveFailedException extends Exception {

    private static final long serialVersionUID = -2449366228226639442L;

    public SaveFailedException(String message, Exception cause) {
        super(message, cause);
    }

    public SaveFailedException(Exception cause) {
        super(cause);
    }
}
