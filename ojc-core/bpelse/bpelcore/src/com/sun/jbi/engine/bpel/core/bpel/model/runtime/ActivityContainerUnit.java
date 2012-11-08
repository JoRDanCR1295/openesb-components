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
 * @(#)ActivityContainerUnit.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.model.runtime;

import com.sun.bpel.model.BPELElement;
import com.sun.jbi.engine.bpel.core.bpel.engine.BusinessProcessInstanceThread;
import com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame;


/**
 * Activity container unit interface
 *
 * @author Sun Microsystems
 */
public interface ActivityContainerUnit extends Unit {
    /**
     * DOCUMENT ME!
     * @param childActUnit TODO
     * @param frame DOCUMENT ME!
     * @param bpit DOCUMENT ME!
     * @param rObjs DOCUMENT ME!
     * @param childActUnit DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     *
     * @throws Exception DOCUMENT ME!
     */
    public boolean doResumeAction(ActivityUnit childActUnit, ICallFrame frame,
        BusinessProcessInstanceThread bpit, RequiredObjects rObjs
    ) throws Exception;
    
    /** returns the corresponding underlying model.  
     * 
     * TODO this should ideally be on the Unit and not in this interface. To put it on the Unit,
     * it needs more refactring and hence is here.
     * @return BPELElement
     */
    BPELElement getStaticModel();
}
