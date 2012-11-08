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
 * @(#)TransactionProcessor.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.common.tale.core.domain.processor;

import com.sun.jbi.common.tale.core.domain.TaleRequest;
import com.sun.jbi.common.tale.core.util.TaleException;

/**
 * Implements the Transaction Processor for ALE.
 * @author Sun MicroSystems
 */
public class TransactionProcessor implements Processor {
    private Processor nextProcessor;

    public TransactionProcessor(Processor myTarget) {
        nextProcessor = myTarget;
    }

    /** @see com.sun.jbi.engine.ale.core.domain.processor.Processor#execute(com.sun.jbi.engine.ale.core.domain.ALERequest) */
    public void execute(TaleRequest request) throws TaleException {
        // To Do: Implement the TransactionProcessor logic here
        
        // And then call the next Processor
        nextProcessor.execute(request);
    }

}
