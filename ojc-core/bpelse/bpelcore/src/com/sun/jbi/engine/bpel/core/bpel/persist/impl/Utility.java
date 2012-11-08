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
 * @(#)Utility.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.persist.impl;

import com.sun.bpel.model.CatchAll;
import com.sun.bpel.model.Flow;
import com.sun.bpel.model.ForEach;
import com.sun.bpel.model.Pick;
import com.sun.bpel.model.Scope;
import com.sun.bpel.model.TerminationHandler;
import com.sun.bpel.xml.common.model.XMLNode;


/**
 * DOCUMENT ME!
 *
 * @author Sun Microsystems
 */
public class Utility {
    /**
     * This is needed since each class in the model doesn't have an unique Id and additions to
     * model which extend other classes can easily pass through the checks of object "instanceof".
     * // TODO need to figure out a way.
     *
     * @param obj Object
     * @param cls Class
     *
     * @return boolean: if cls is an instance of obj, returns true; otherwise, returns false
     */
    public static boolean checkClassType(Object obj, Class cls) {
        return (cls.isInstance(obj)); // && obj.getClass().getName().equals(cls.getName()));
    }

    /**
     * check if a string is empty
     *
     * @param str String
     *
     * @return boolean: if str is empty, returns true; otherwise, returns false
     */
    public static boolean isEmpty(String str) {
        return (str == null) || str.trim().equals(""); //$NON-NLS-1$
    }
    
    public static boolean isFlowPersistenceRequired(XMLNode xmlNode) {
    	
    	if(xmlNode == null) {
    		return false;
    	} else if(xmlNode instanceof CatchAll) {
    		return true;
    	} else if(xmlNode instanceof TerminationHandler) {
    		return true;
    	} else if(xmlNode instanceof Flow) {
    		return false;
    	} else if(xmlNode instanceof Scope && ((Scope)xmlNode).getEventHandlers() != null) {
    		return false;
    	} else if(xmlNode instanceof ForEach) {
    		return false;
    	} else if(xmlNode instanceof Pick) {
    		return false;
    	} else {
    		return isFlowPersistenceRequired(xmlNode.getParent());
    	} 
    }
}
