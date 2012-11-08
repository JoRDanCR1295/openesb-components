/*
 * @(#)RuntimeIOException.java        $Revision: 1.1.1.1 $ $Date: 2008/06/30 08:53:20 $
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

import java.io.IOException;

/**
 * Convenience class for rethrowing an <code>IOException</code> as an unchecked exception.
 * 
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.1.1.1 $ $Date: 2008/06/30 08:53:20 $
 * 
 * @see java.io.IOException
 * @since 0.1
 */
public final class RuntimeIOException extends RuntimeException {
    
    private static final long serialVersionUID = -2449366228226639442L;

    public RuntimeIOException(final IOException cause) {
        super(cause);
    }

    public RuntimeIOException(final String message, final IOException cause) {
        super(message, cause);
    }

    @Override
    public IOException getCause() {
        return (IOException) super.getCause();
    }
}
