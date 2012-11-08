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
  * $Id: GetGUID.java,v 1.1 2008/09/27 00:37:10 malkit Exp $
  *
  * Copyright 2006 Sun Microsystems, Inc. All Rights Reserved.  */

package com.sun.jbi.engine.bpel.core.bpel.xpath.functions;

import org.apache.commons.jxpath.ExpressionContext;
import org.apache.commons.jxpath.Function;

import com.sun.jbi.engine.bpel.core.bpel.util.BPELHelper;

public class GetGUID implements Function {

	public Object invoke(ExpressionContext arg0, Object[] arg1) {
		return BPELHelper.getGUID();
	}
}
