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
 * @(#)NDC.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.crl.util;

import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Utility to enter and exit a nested diagnostic context.
 * Instances of this class can be used as an NDC marker,
 * if for some reason the static methods are insufficient.
 * 
 * @author Kevan Simpson
 */
public class NDC {
	private static Logger mNDC = Logger.getLogger(NDC.class.getName());
	private static Logger mEnter = Logger.getLogger("com.sun.EnterContext");
	private static Logger mLeave = Logger.getLogger("com.sun.ExitContext");
	
	private final Object[] mCtx;	// simple context or key-value pairs
	
	/** Constructs an NDC instance with the specified context. */
	protected NDC(Object... ctx) {
		mCtx = ctx;
	}
	
	/**
	 * Enters a diagnostic context and returns a NDC marker.
	 * 
	 * @param ctx One or more context objects.
	 * @return an NDC marker for the specified context.
	 * @throws IllegalArgumentException if any context object is <code>null</code>.
	 */
	public static NDC enter(Object... ctx) {
		return new NDC(ctx).enter();
	}
	
	/**
	 * Exits the specified diagnostic context.
	 * 
	 * @param ctx The name of the diagnostic context.
	 * @param params Any message parameters to the context.
	 * @throws IllegalArgumentException if any context object is <code>null</code>.
	 */
	public static void exit(Object... ctx) {
		String str = buildString(ctx);
		mLeave.log(Level.FINE, str, ctx);
		if (mNDC.isLoggable(Level.FINEST)) {
			mNDC.log(Level.FINEST, "CRL-1002: Exiting NDC "+ str, ctx);
		}
	}

	/**
	 * Enters this marker's diagnostic context.
	 * @return this NDC instance.
	 */
	protected NDC enter() {	// protected cuz not sure this should be exposed
		String str = buildString(mCtx);
		mEnter.log(Level.FINE, str, mCtx);
		if (mNDC.isLoggable(Level.FINEST)) {
			mNDC.log(Level.FINEST, "CRL-1001: Entering NDC "+ str, mCtx);
		}
		return this;
	}
	
	/**
	 * Exits this marker's diagnostic context.
	 */
	public void exit() {
		exit(mCtx);
	}
	
	private static String buildString(Object[] ctx) {
		StringBuffer buff = new StringBuffer();
		if (ctx == null || ctx.length == 0) {
			throw new IllegalArgumentException(I18n.loc(
					"CRL-6052: Invalid context, may not be NULL or zero length!"));
		}
		
		int len = ctx.length;
		if (len == 1) {
			if (ctx[0] == null) {
				throw new IllegalArgumentException(I18n.loc(
						"CRL-6052: Invalid context, may not be NULL or zero length!"));
			}
			else {
				buff.append("Context={0}");
			}
		}
		else if ((len % 2) == 0) {	// even count, key-value pairs
			buff.append("Context: ");
			boolean comma = false;
			// ndc prints out key values in backwards order, so shall we
			for (int i = (len - 1); i >= 0; i -= 2) {
				if (comma) {
					buff.append(",");
				} else {
					comma = true;
				}
				buff.append("{").append((i - 1)).append("}={").append(i).append("}");
			}
		}
		else {
			throw new IllegalArgumentException(I18n.loc(
					"CRL-6051: Invalid context, must be in key-value pairs!"));
		}
		
		return buff.toString();
	}
}
