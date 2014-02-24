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
 * @(#)$Id: VariableImpl.java,v 1.8 2009/03/07 02:10:11 vinayram Exp $
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.event.impl;

import javax.xml.namespace.QName;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

import com.sun.bpel.model.meta.RVariable;
import com.sun.jbi.engine.bpel.core.bpel.event.Variable;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.VariableScope;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.WSMessage;
import com.sun.jbi.engine.bpel.core.bpel.util.DOMHelper;
import java.text.CharacterIterator;
import java.text.StringCharacterIterator;


public class VariableImpl implements Variable {

	private long mScopeId;
	private DataType mType;
	private String mValue;
	private long mVarId;
	private String mVarName;
	private QName mFaultName;
	private VariableScope mVariableScope;
	
	public VariableImpl(long scopeId, DataType type, String value, long varId, QName varName) {
		mScopeId = scopeId;
		mType = type;
		mValue = value;
		mVarId = varId;
		mFaultName = varName;
	}	

	public VariableImpl (RVariable rvar, Object variableData, VariableScope varScope) {
		mScopeId = rvar.getScopeId();
		mVariableScope = varScope;
		if (rvar.getMessageType() != null) {
			mType = DataType.Message;
		} else 	if (rvar.getElement() != null) {
			mType = DataType.Element;
		} else if (rvar.getXSDType() != null) {
			if (!rvar.getXSDType().isSimpleType()) {
				mType = DataType.Complex_Type;
			}else if (rvar.isBoolean()) {
				mType = DataType.Boolean;
			} else if (rvar.isDate() || rvar.isDateTime() || rvar.isTime()) {
				mType = DataType.Date;
			} else if (rvar.isNumber()) {
				mType = DataType.Number;
			} else if (rvar.isString()) {
				mType = DataType.String;
			} else {
				mType = DataType.Simple_Type_Other;
			}
		}
		mVarId = rvar.getUniqueId();
		mVarName = rvar.getName();				

		if (variableData != null) {
            if (variableData instanceof Element) {
                mValue = DOMHelper.createXmlString((Element) variableData);
            } else if (variableData instanceof Document) {
            	mValue = DOMHelper.createXmlString(((Document) variableData)
                        .getDocumentElement());
            } else {
                mValue = variableData.toString();
                // remove trailing zero if is integer.
                if (rvar.isInteger()) {
                	Double value = new Double(mValue);
                	if(((value.doubleValue()- Math.floor(value))==0)){
                		Long longValue  = new Long(value.longValue());
                		mValue = longValue.toString();
                	} 
                }            	
            }
        }
	}
	
	public VariableImpl(RVariable rvar, WSMessage value, VariableScope varScope) {		
		mScopeId = rvar.getScopeId();
		mVariableScope = varScope;
		mType = DataType.Message;
		if (value != null) {
			mValue = value.toString();
		}
		mVarId = rvar.getUniqueId();
		mVarName = rvar.getName();		
	}

	public long getScopeId() {
		// TODO Auto-generated method stub
		return mScopeId;
	}

	public DataType getType() {
		// TODO Auto-generated method stub
		return mType;
	}

	public String getValue() {
		// TODO Auto-generated method stub
		return mValue;
	}

	public long getVarId() {
		// TODO Auto-generated method stub
		return mVarId;
	}

	public String getVarName() {
		// TODO Auto-generated method stub
		return mVarName;
	}

	public QName getFaultName() {
		// TODO Auto-generated method stub
		return mFaultName;
	}

	public boolean isFault() {
		// TODO Auto-generated method stub
		return mFaultName != null;
	}

	@Override
	public String toString() {
		// TODO Auto-generated method stub
		StringBuffer buffer = new StringBuffer ();
		buffer.append("---------------Variable:");
		if (!isFault ()) {
			buffer.append(mVarName);
		} else {
			buffer.append(mFaultName);
		}
		buffer.append("---------------\n");
		buffer.append("ScopeId:");
		buffer.append(mScopeId);
		buffer.append("\n");
		
		buffer.append("DataType:");
		buffer.append(mType);
		buffer.append("\n");		
		
		buffer.append("Value:");
		buffer.append(mValue);
		buffer.append("\n");				
		
		buffer.append("VarId:");
		buffer.append(mVarId);
		buffer.append("\n");
		
		buffer.append("VarName:");
		buffer.append(mVarName);
		buffer.append("\n");		
		
		buffer.append("FaultName:");
		buffer.append(mFaultName);
		buffer.append("\n");				
		
		buffer.append("---------------------------------------------\n");
		return buffer.toString();
	}


    public String toXML() {
        // TODO Auto-generated method stub
        StringBuffer buffer = new StringBuffer();

        buffer.append("<msgns:variable>\n");
        if (!isFault()) {
            buffer.append("<msgns:name>" + mVarName + "</msgns:name>\n");
        } else {
            buffer.append("<msgns:name>" + mFaultName + "</msgns:name>\n");
        }
        buffer.append("<msgns:scopeid>" + mScopeId + "</msgns:scopeid>\n");
        buffer.append("<msgns:datatype>" + mType + "</msgns:datatype>\n");
        buffer.append("<msgns:value>" + forXML(mValue) + "</msgns:value>\n");
        buffer.append("<msgns:varid>" + mVarId + "</msgns:varid>\n");
        buffer.append("<msgns:varname>" + mVarName + "</msgns:varname>\n");
        buffer.append("<msgns:varfault>" + mFaultName + "</msgns:varfault>\n");

        buffer.append("</msgns:variable>\n");
        return buffer.toString();
    }

    protected static String forXML(String aText) {
        final StringBuilder result = new StringBuilder();
        final StringCharacterIterator iterator = new StringCharacterIterator(aText);
        char character = iterator.current();
        while (character != CharacterIterator.DONE) {
            if (character == '<') {
                result.append("&lt;");
            } else if (character == '>') {
                result.append("&gt;");
            } else if (character == '\"') {
                result.append("&quot;");
            } else if (character == '\'') {
                result.append("&apos;");
            } else if (character == '&') {
                result.append("&amp;");
            } else {
                //the char is not a special one
                //add it to the result as is
                result.append(character);
            }
            character = iterator.next();
        }
        return result.toString();
    }
        
    public VariableScope getVariableScope() {
        // TODO Auto-generated method stub
        return mVariableScope;
    }

}
