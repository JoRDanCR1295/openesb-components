/*
 * @(#)FileAlreadyExistsException.java        $Revision: 1.1 $ $Date: 2008/10/25 22:02:56 $
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

package org.openesb.components.rules4jbi.netbeans.project.fileimport;

import java.io.IOException;

/**
 * This exception is thrown when a user tries to import a file
 * into the project's folder, where a file with the same name already exists.
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.1 $ $Date: 2008/10/25 22:02:56 $
 * 
 * @since 0.3
 */
public class FileAlreadyExistsException extends IOException {
    
    private static final long serialVersionUID = -2449366228226639442L;

    public FileAlreadyExistsException(String message) {
        super(message);
    }
    
    public FileAlreadyExistsException(String message, Exception cause) {
        super(message, cause);
    }
    
    public FileAlreadyExistsException(Exception cause) {
        super(cause);
    }
}
