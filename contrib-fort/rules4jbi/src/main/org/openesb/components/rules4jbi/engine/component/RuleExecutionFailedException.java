/*
 * @(#)RuleExecutionFailedException.java        $Revision: 1.2 $ $Date: 2008/07/14 16:30:26 $
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

/**
 * This exception indicates that a rule execution has failed.
 * The underlying cause gives more details about the failure.
 * 
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.2 $ $Date: 2008/07/14 16:30:26 $
 * 
 * @since 0.1
 */
public class RuleExecutionFailedException extends Exception {
    
    private static final long serialVersionUID = -2449366228226639442L;

    public RuleExecutionFailedException(final Exception cause) {
        super(cause);
    }
    
    @Override
    public Exception getCause() {
        return (Exception) super.getCause();
    }
}
