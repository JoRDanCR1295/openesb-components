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
 * @(#)I18N.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.monitor.tool.util;

import java.util.regex.Pattern;

import com.sun.jbi.common.util.LocalizationSupport;


/**
 * Internationalization utility for BPEL Core.
 * @author Sun Microsystems
 */
public class I18n extends LocalizationSupport {
	private static final I18n mI18n = new I18n();
	
	protected I18n() {
		super(Pattern.compile("(BPCOR-[4-7]\\d\\d\\d)(: )(.*)", Pattern.DOTALL), 
			  "", null);
	}
	
	public static String loc(String message, Object... params) {
		return mI18n.t(message, params);
	}
}

