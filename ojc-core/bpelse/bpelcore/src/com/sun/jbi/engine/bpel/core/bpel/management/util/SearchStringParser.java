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
 * @(#)SearchStringParser.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.bpel.core.bpel.management.util;

import java.util.ArrayList;
import java.util.List;
import java.util.Stack;

import com.sun.jbi.engine.bpel.core.bpel.management.util.SearchLevel.Operator;
import com.sun.jbi.engine.bpel.core.bpel.util.I18n;

/**
 * @author Sun Microsystems
 *
 */
public class SearchStringParser {
	
	/* */
	private static final String OB = "(";
	
	/* */
	private static final String CB = ")";
	
	/* */
	private static final String COMMA = ",";
	
	/* */
	private static final String AND = "AND";
	
	/* */
	private static final String OR = "OR";

	/* */
	private SearchStringTokenizer mTokeizer;
	
	/* */
	private Stack<SearchLevel> mStack;
	
	/* */
	private SearchLevel mCurrentLevel;
	
	/* */
	private String mSearchString;
	
	/* */
	private List<String> mStrList;
	
	/**
	 * 
	 */
	public SearchStringParser(String searchString) {
		mSearchString = searchString;
		mStack = new Stack<SearchLevel>();
		mStrList = new ArrayList<String>();
	}
	
	/**
	 * 
	 * @return
	 */
	public SearchUnit parseSearchString() {
		mTokeizer = new SearchStringTokenizer(mSearchString);
		mStack.clear();
		mStrList.clear();
		String token = mTokeizer.nextToken();
		mCurrentLevel = new SearchLevel();
		
		if (token == null) {
			throw new RuntimeException(I18n.loc("BPCOR-6184: Unexcepted end of search string. {0}", mSearchString));
		} else if (token.equals(OB)) {
			processOB();
		} else if ((token.equals(COMMA)) || (token.equals(CB))) {
			String expected = OB + " or Search part parameter";
			throw new RuntimeException(I18n.loc("BPCOR-6185: Unexpected token {0}. Expecting {1}. " +
					"Search string {2}", token, expected, mSearchString));
		} else {
			processString(token);
		}
		
		if (!mStack.isEmpty()) {
			throw new RuntimeException(I18n.loc("BPCOR-6184: Unexcepted end of search string. {0}", mSearchString));
		}
		
		return mCurrentLevel.purge();
	}

	/*
	 * 
	 */
	private void processOB() {
		SearchLevel searchLevel = new SearchLevel();
		mCurrentLevel.setSearchPart(searchLevel);
		mStack.push(mCurrentLevel);
		mCurrentLevel = searchLevel;
		
		String token = mTokeizer.nextToken();

		if (token == null) {
			throw new RuntimeException(I18n.loc("BPCOR-6184: Unexcepted end of search string. {0}", mSearchString));
		} else if (token.equals(OB)) {
			processOB();
		} else if ((token.equals(COMMA)) || (token.equals(CB))) {
			String expected = OB + " or Search part parameter";
			throw new RuntimeException(I18n.loc("BPCOR-6185: Unexpected token {0}. Expecting {1}. " +
					"Search string {2}", token, expected, mSearchString));
		} else {
			processString(token);
		}
	}

	/*
	 * 
	 */
	private void processString(String strVal) {
		mStrList.add(strVal);
		
		String token = mTokeizer.nextToken();

		if (token == null) {
			if (!mStack.isEmpty()) {
				throw new RuntimeException(I18n.loc("BPCOR-6184: Unexcepted end of search string. {0}", mSearchString));
			}
			createSearchPart();
		} else if (token.equals(COMMA)) {
			processComma();
		} else if (token.equals(CB)) {
			createSearchPart();
			processCB();
		} else {
			String expected = COMMA + " or " + CB ;
			throw new RuntimeException(I18n.loc("BPCOR-6185: Unexpected token {0}. Expecting {1}. " +
					"Search string {2}", token, expected, mSearchString));
		}

	}

	/*
	 *
	 */
	private void processComma() {
		String token = mTokeizer.nextToken();

		if (token == null) {
			throw new RuntimeException(I18n.loc("BPCOR-6184: Unexcepted end of search string. {0}", mSearchString));
		} else if ((token.equals(OB)) || (token.equals(COMMA)) || (token.equals(CB))) {
			String expected = "Search part parameter";
			throw new RuntimeException(I18n.loc("BPCOR-6185: Unexpected token {0}. Expecting {1}. " +
					"Search string {2}", token, expected, mSearchString));
		} else {
			processString(token);
		}
	} 

	/*
	 * 
	 */
	private void processCB() {
		if (!mStack.isEmpty()) {
			mCurrentLevel = mStack.pop();
		} else {
			String expected =  OR + ", " + AND + " or end of search string";
			throw new RuntimeException(I18n.loc("BPCOR-6185: Unexpected token {0}. Expecting {1}. " +
					"Search string {2}", CB, expected, mSearchString));
		}
		
		String token = mTokeizer.nextToken();

		if (token == null) {
			if (!mStack.isEmpty()) {
				throw new RuntimeException(I18n.loc("BPCOR-6184: Unexcepted end of search string. {0}", mSearchString));
			}
		} else if (token.equals(CB)) {
			processCB();
		} else if (token.equalsIgnoreCase(AND)) {
			processOper(Operator.AND);
		} else if (token.equalsIgnoreCase(OR)) {
			processOper(Operator.OR);
		} else {
			String expected = OR + ", " + AND + " or " + CB;
			throw new RuntimeException(I18n.loc("BPCOR-6185: Unexpected token {0}. Expecting {1}. " +
					"Search string {2}", token, expected, mSearchString));
		}
	}

	/*
	 * 
	 */
	private void processOper(Operator oper) {
		if (mCurrentLevel.getOperator() == null) {
			mCurrentLevel.setOperator(oper);
		} else if (!mCurrentLevel.getOperator().equals(oper)) {
			throw new RuntimeException(I18n.loc("Ambiguity in search string. Operator {0} followed by {1} at " +
					"same level. Search string {2}", mCurrentLevel.getOperator().toString(), 
					oper.toString(), mSearchString));
		}
		
		String token = mTokeizer.nextToken();

		if (token == null) {
			throw new RuntimeException(I18n.loc("BPCOR-6184: Unexcepted end of search string. {0}", mSearchString));
		} else if (token.equals(OB)) {
			processOB();
		} else {
			throw new RuntimeException(I18n.loc("BPCOR-6185: Unexpected token {0}. Expecting {1}. " +
					"Search string {2}", token, OB, mSearchString));
		}
	}
	
	/*
	 * 
	 */
	private void createSearchPart() {
		// Post processing.
		if (mStrList.size() > 4) {
			StringBuilder searchPart = new StringBuilder();
			boolean notFirst = false;
			for (String val : mStrList) {
				if (notFirst) {
					searchPart.append(", ");
				} else {
					notFirst = true;
				}
				searchPart.append(val);
			}
			throw new RuntimeException(I18n.loc("BPCOR-6186: More that expected parameters for search part. " +
					"Search Part: {0}. Search string: {1}", searchPart.toString(), mSearchString));
		} 
		String[] strArr = new String[4];
		int i = 0;
		for (String val : mStrList) {
			strArr[i++] = val;
		}
		try {
			SearchPart param = new SearchPart(strArr[0], strArr[1], strArr[2], strArr[3]);
			mCurrentLevel.setSearchPart(param);
		} catch (RuntimeException re) {
			StringBuilder searchPart = new StringBuilder();
			boolean notFirst = false;
			for (String val : mStrList) {
				if (notFirst) {
					searchPart.append(", ");
				} else {
					notFirst = true;
				}
				searchPart.append(val);
			}
			throw new RuntimeException(I18n.loc("BPCOR-6187: Error parsing search Type. Error: {0} Search part: {1}." +
					" Search string: {2}", re.getLocalizedMessage(), searchPart.toString(), mSearchString));
		}
		mStrList.clear();
	}
}
