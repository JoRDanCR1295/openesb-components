/*
 * @(#)InvalidDirectoryStructureException.java        $Revision: 1.1.1.1 $ $Date: 2008/06/30 08:53:20 $
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

package org.openesb.components.rules4jbi.netbeans.project.directory;

/**
 * This exception indicates that the project's directory structure is
 * in an inconsistent state, probably after modifications by user from
 * outside of the IDE. While some of the project's artifacts might still
 * exist on the disk, the project is no more usable.
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.1.1.1 $ $Date: 2008/06/30 08:53:20 $
 * 
 * @since 0.1
 */
public class InvalidDirectoryStructureException extends RuntimeException {
    
    private static final long serialVersionUID = -2449366228226639442L;

    public InvalidDirectoryStructureException(String message) {
        super(message);
    }
    
    public InvalidDirectoryStructureException(String message, Exception cause) {
        super(message, cause);
    }
    
    public InvalidDirectoryStructureException(Exception cause) {
        super(cause);
    }
}
