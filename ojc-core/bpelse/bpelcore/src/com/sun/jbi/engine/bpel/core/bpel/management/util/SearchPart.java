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
 * @(#)SearchPart.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.bpel.core.bpel.management.util;

import java.util.GregorianCalendar;

import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.DatatypeFactory;
import javax.xml.datatype.XMLGregorianCalendar;

import com.sun.jbi.engine.bpel.core.bpel.persist.MonitorDBSchemaCreation;
import com.sun.jbi.engine.bpel.core.bpel.util.I18n;


/**
 * @author Sun Microsystems
 *
 */
public class SearchPart extends SearchUnit {
	
	/** */
	public enum SearchType {
		StringEquals("eq"), 
		StringHas("cs"), 
		NotEquals("ne"), 
		NumericRange("nr"), 
		DateRange("dr");
		
		String mCommand;
		SearchType(String command) {
			mCommand = command;
		}
		
		public String getCommandString() {
			return mCommand;
		}
		
		public static SearchType parseCommand(String command) {
			SearchType retType = null;
			for (SearchType type : SearchType.values()) {
				if (type.getCommandString().equalsIgnoreCase(command)) {
					retType = type;
					break;
				}
			}
			return retType;
		}
		
	}
	
	/* */
	private Type mSearchPartType;
	
	/* */
	private Long mVarId;
	
	/* */
	private SearchType mSearchType;
	
	/* */
	private String mVal1;
	
	/* */
	private String mVal2;
	
	/**
	 * 
	 * @param param1
	 * @param param2
	 * @param param3
	 * @param param4
	 */
	public SearchPart(String param1, String param2, String param3, String param4) {
		mSearchPartType = Type.Params;
		validateAndInitialize(param1, param2, param3, param4);
	}
	
	/**
	 * 
	 * @return
	 */
	public Long getVariableId() {
		return mVarId;
	}

	/**
	 * 
	 * @return
	 */
	public SearchType getSearchType() {
		return mSearchType;
	}
	
	/**
	 * 
	 * @return
	 */
	public String getValue1() {
		return mVal1;
	}
	
	/**
	 * 
	 * @return
	 */
	public String getValue2() {
		return mVal2;
	}
	
	/* (non-Javadoc)
	 * @see com.sun.jbi.engine.bpel.core.bpel.management.util.SearchUnit#purge()
	 */
	@Override
	public SearchUnit purge() {
		return this;
	}

	/* (non-Javadoc)
	 * @see com.sun.jbi.engine.bpel.core.bpel.management.SearchPart#generateSQL()
	 */
	public String generateSQL() {
		StringBuilder builder = new StringBuilder();

		builder.append("(SELECT "); 
		builder.append(SchemaConstants.INSTANCE_ID); 
		builder.append(" FROM "); 
		builder.append(MonitorDBSchemaCreation.MONITOR_SIMPLE_VARIABLE);
		builder.append(" WHERE ");
		builder.append(SchemaConstants.VAR_ID);
		builder.append(" = ");
		builder.append(mVarId);
		builder.append(" AND ");
		
		switch(mSearchType) {
			case StringEquals:
				builder.append(SchemaConstants.STR_VALUE);
				builder.append(" = '");
				builder.append(mVal1);
				builder.append("'");
				break;
			case StringHas:
				builder.append(SchemaConstants.STR_VALUE);
				builder.append(" LIKE '%");
				builder.append(mVal1);
				builder.append("%'");
				break;
			case NotEquals:
				builder.append(SchemaConstants.STR_VALUE);
				builder.append(" <> '");
				builder.append(mVal1);
				builder.append("'");
				break;
			case DateRange:
				// DEVNOT: Dates are stored in their long representation in the 'numval' column also
				// We will be working against this column for date ranges. As such the query for
				// numeric range and date range is the same.
			case NumericRange:
				builder.append(SchemaConstants.NUM_VALUE);
				builder.append(" >= ");
				builder.append(mVal1);
				builder.append(" AND ");
				builder.append(SchemaConstants.NUM_VALUE);
				builder.append(" <= ");
				builder.append(mVal2);
				break;
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
	
	/*
	 * 
	 */
	private void validateAndInitialize(String param1, String param2, String param3, String param4) {
		if (param1 != null) {
			try {
				mVarId = Long.parseLong(param1);
			} catch (NumberFormatException nfe) {
				throw new RuntimeException(I18n.loc("BPCOR-6128: Variable Id {0} is not a number.", param1));
			}
		} else {
			throw new RuntimeException(I18n.loc("BPCOR-6159: Variable Id's cannot be null"));
		}
		
		if (param2 != null) {
			if ((param3 == null) && (param4 == null)) {
				mVal1 = param2;
				mSearchType = SearchType.StringEquals;
				return;
			} else {
				mSearchType = SearchType.parseCommand(param2);
				if (mSearchType == null) {
					throw new RuntimeException(I18n.loc("BPCOR-6160: {0} is not a valid search type.", param2));
				}
			}
		} else {
			throw new RuntimeException(I18n.loc("BPCOR-6164: Insufficient Parameters in search part: {0}", param1));
		}
		
		if (param3 != null) {
			if ((mSearchType.equals(SearchType.StringEquals)) 
					|| (mSearchType.equals(SearchType.StringHas))
					|| (mSearchType.equals(SearchType.NotEquals))) {
				mVal1 = param3;
				return;
			} else if (mSearchType.equals(SearchType.NumericRange)) {
				try {
					Double.parseDouble(param3);
					mVal1 = param3;
				} catch (NumberFormatException nfe) {
					String searchPart = param1 + ", " + param2 + ", " + param3 + ", " + param4;
					throw new RuntimeException(
							I18n.loc("BPCOR-6167: {0} is an invalid number. Search part: {1}", param3, searchPart));
				}
			} else if (mSearchType.equals(SearchType.DateRange)) {
				try {
					mVal1 = (new Long(getXSDDateAsMillis(param3))).toString();
				} catch (Exception re) {
					String searchPart = param1 + ", " + param2 + ", " + param3 + ", " + param4;
					throw new RuntimeException(I18n.loc("BPCOR-6170: Error parsing xsd date {0}. Search part: {1}. " +
							"Error: {2}", param3, searchPart, re.getLocalizedMessage()));
				}
			}
		} else {
			String searchPart = param1 + ", " + param2;
			throw new RuntimeException(I18n.loc("BPCOR-6164: Insufficient Parameters in search part: {0}", searchPart));
		}
		
		if (param4 != null) {
			if (mSearchType.equals(SearchType.NumericRange)) {
				try {
					Double.parseDouble(param4);
					mVal2 = param4;
				} catch (NumberFormatException nfe) {
					String searchPart = param1 + ", " + param2 + ", " + param3 + ", " + param4;
					throw new RuntimeException(
							I18n.loc("BPCOR-6167: {0} is an invalid number. Search part: {1}", param4, searchPart));
				}
			} else if (mSearchType.equals(SearchType.DateRange)) {
				try {
					mVal2 = (new Long(getXSDDateAsMillis(param4))).toString();
				} catch (Exception re) {
					String searchPart = param1 + ", " + param2 + ", " + param3 + ", " + param4;
					throw new RuntimeException(I18n.loc("BPCOR-6170: Error parsing xsd date {0}. Search part: {1}. " +
							"Error: {2}", param4, searchPart, re.getLocalizedMessage()));
				}
			}
		} else {
			String searchPart = param1 + ", " + param2 + ", " + param3;
			throw new RuntimeException(I18n.loc("BPCOR-6164: Insufficient Parameters in search part: {0}", searchPart));
		}
	}
	
	/*
	 * 
	 */
	private long getXSDDateAsMillis(String strValue) throws DatatypeConfigurationException {
		
		XMLGregorianCalendar xmlCal = DatatypeFactory.newInstance().newXMLGregorianCalendar(strValue);
		GregorianCalendar gCal = xmlCal.toGregorianCalendar();
		
		return gCal.getTimeInMillis();
	}

}
