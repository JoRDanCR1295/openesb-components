/*
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://glassfish.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://glassfish.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */
 /*
  * $Id: BPWSExtFunctions.java,v 1.2 2008/09/27 00:37:09 malkit Exp $
  *
  * Copyright 2006 Sun Microsystems, Inc. All Rights Reserved.  */

package com.sun.jbi.engine.bpel.core.bpel.xpath.functions;

import java.util.Set;

import org.apache.commons.jxpath.Function;
import org.apache.commons.jxpath.Functions;

/**
 * BPEL Extension function set to be added to JXPath context.
 *
 * @author Sun Microsystems
 */
public class BPWSExtFunctions implements Functions {
    private static final Function doMarshal = new XSDMarshal();
    private static final Function doUnMarshal = new XSDUnMarshal();
    private static final Function mGetGUID = new GetGUID();
    private Function mGetBPId;

    public BPWSExtFunctions(String bpInstanceId) {
    	this.mGetBPId = new GetBPID(bpInstanceId);
	}

	public Function getFunction(String namespace, String name,
            Object[] parameters) {
        if (name.equals("doMarshal")) {
            return doMarshal;
        } else if (name.equals("doUnMarshal")) {
            return doUnMarshal;
        } else if (name.equals("getGUID")) {
        	return mGetGUID;
        } else if (name.equals("getBPId")) {
        	return mGetBPId;
        }
        return null;
    }

    public Set getUsedNamespaces() {
        return null;
    }

}
