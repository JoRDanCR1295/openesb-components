/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)StaticOperation.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.workflow.clientapi;

import org.w3c.dom.Element;

/**
 * Client operation are represented by this interface.
 * This encapsulates the logic invoked for each client
 * operation.
 * 
 */
public interface StaticOperation {
 
    /**
     * execute client operation logic
     */
    void execute();
    
    /**
     * get the result of this client operation
     * which should be converted to NMR  normalized message format
     */
    StaticOperationReply getOutput();
    
    static class StaticOperationReply {
        private Element mReply;
        private Element mFault;
        private boolean mFaulted = false;
        public Element getFault() {
            return mFault;
        }
        public void setFault(Element fault) {
            mFault = fault;
        }
        public boolean isFaulted() {
            return mFaulted;
        }
        public void setFaulted(boolean faulted) {
            mFaulted = faulted;
        }
        public Element getReply() {
            return mReply;
        }
        public void setReply(Element reply) {
            mReply = reply;
        }
        
        
    }
}
