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
 * @(#)SearchTokenizer.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.bpel.core.bpel.management.util;

import com.sun.jbi.engine.bpel.core.bpel.util.I18n;

/**
 * @author Sun Microsystems
 *
 */
public class SearchStringTokenizer {
	
	/* */
	private char[] mChars;
	
	/* */
	private String mToken;
	
	/* */
	private int mCount;

	/**
	 * 
	 * @param searchString
	 */
	public SearchStringTokenizer(String searchString) {
		if (searchString == null) {
			throw new RuntimeException(I18n.loc("BPCOR-6183: Search string is null."));
		}
		mChars = searchString.toCharArray();
		mCount = 0;
		mToken = getNextToken();
	}
	
	/**
	 * 
	 * @return
	 */
	public String nextToken() {
		String nextToken = mToken;
		String tempToken = null;
		do {
			tempToken = getNextToken();
			if (tempToken == null) {
				break;
			} 
		} while (tempToken.length() == 0);
		mToken = tempToken;
		
		return nextToken;
	}
	
	/**
	 * 
	 * @return
	 */
	public String peek() {
		return mToken;
	}
	
	/*
	 * 
	 */
	private String getNextToken() {
		if (mCount >= mChars.length) {
			return null;
		}
		
		boolean loop = true;
		StringBuilder builder = new StringBuilder();
		while (loop) {
			if (mCount >= mChars.length) {
				loop = false;
			} else if ((mChars[mCount] == '(') 
					|| (mChars[mCount] == ')')
						|| (mChars[mCount] == ','))
			{
				if (builder.length() > 0) {
					loop = false;
				} else {
					builder.append(mChars[mCount]);
					mCount++;
					loop = false;
				}
			} else {
				builder.append(mChars[mCount]);
				mCount++;
			}
		}
		
		return builder.toString().trim();
	}
}
