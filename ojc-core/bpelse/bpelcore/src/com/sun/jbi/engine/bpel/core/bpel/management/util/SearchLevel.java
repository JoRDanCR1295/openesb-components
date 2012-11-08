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
 * @(#)SearchLevel.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.bpel.core.bpel.management.util;

import java.util.ArrayList;
import java.util.List;

/**
 * @author Sun Microsystems
 *
 */
public class SearchLevel extends SearchUnit {
	
	/** */
	public enum Operator {AND, OR}
	
	/* */
	private Type mSearchPartType;
	
	/* */
	private Operator mOperator;
	
	/* */
	private List<SearchUnit> mPartList;
	
	/**
	 * 
	 */
	public SearchLevel() {
		mSearchPartType = Type.Level;
		mPartList = new ArrayList<SearchUnit>();
	}

	/**
	 * 
	 * @param operator
	 */
	public void setOperator(Operator operator) {
		mOperator = operator;
	}
	
	/**
	 * 
	 * @return
	 */
	public Operator getOperator() {
		return mOperator;
	}
	
	/**
	 * 
	 * @param searchUnit
	 */
	public void setSearchPart(SearchUnit searchUnit) {
		mPartList.add(searchUnit);
		searchUnit.setParent(this);
	}
	
	/**
	 * 
	 * @return
	 */
	public List<SearchUnit> getSearchParts() {
		return mPartList;
	}
	
	/* (non-Javadoc)
	 * @see com.sun.jbi.engine.bpel.core.bpel.management.util.SearchUnit#purge()
	 */
	@Override
	public SearchUnit purge() {
		SearchUnit parent = getParent();
		// When the operator is null then it means that there will be only one and only one child.
		if (mOperator == null) {
			SearchUnit part = mPartList.get(0);
			part = part.purge();
			part.setParent(parent);
			return part;
		}
		
		for (int i = 0; i < mPartList.size(); i++) {
			SearchUnit part = mPartList.get(i);
			part = part.purge(); 
			mPartList.set(i, part);
		}
		
		return this;
	}

	/* (non-Javadoc)
	 * @see com.sun.jbi.engine.bpel.core.bpel.management.SearchPart#generateSQL()
	 */
	public String generateSQL() {
		StringBuilder builder = new StringBuilder();
		boolean subsequent = false;
		
		String operation;
		if (mOperator == null) {
			operation = " NULL ";
		} else if (mOperator.equals(Operator.AND)) {
			operation = " INTERSECT ";
		} else if (mOperator.equals(Operator.OR)) {
			operation = " UNION ";
		} else {
			operation = " UNKNOWN ";
		}
		
		builder.append("(");
		for (SearchUnit part : mPartList) {
			if (subsequent) {
				builder.append(operation);
			} else {
				subsequent = true;
			}
			builder.append(part.generateSQL());
		}
		builder.append(")");
		
		return builder.toString();
	}

	/* (non-Javadoc)
	 * @see com.sun.jbi.engine.bpel.core.bpel.management.SearchPart#getPartType()
	 */
	public Type getPartType() {
		return mSearchPartType;
	}

}
