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
 * @(#)RuntimeVariableImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl;

import java.io.Reader;

import org.apache.commons.jxpath.Pointer;
import org.apache.commons.jxpath.ri.InfoSetUtil;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

import com.sun.bpel.model.meta.RVariable;
import com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessInstance;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimeVariable;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.WSMessage;
import com.sun.jbi.engine.bpel.core.bpel.util.DOMHelper;
import com.sun.jbi.engine.bpel.core.bpel.util.Utility;
import java.math.BigDecimal;

/**
 * DOCUMENT ME!
 *
 * @author Sun Microsystems
 */
public class RuntimeVariableImpl implements RuntimeVariable {
    private static final String EMPTY_STRING = "";
	private BPELProcessInstance mInstance;
    private boolean mIsInserted;
    private WSMessage mWSMsg;
    private Object mXsdData;
    private RVariable mVarDef;
    private String mScopeGuid;

    private boolean mIsPersisted;
    private boolean mIsPassivated;
    
    private Object scalabilityLock = new Object();
    
    /**
     * Creates a new RuntimeVariableImpl object.
     * @param variable The variable corresponding to which a RuntimeVariables needs to be created
     * @param scopeId: Every variable in the bpel document lives in the context of a scope. This 
     * is not the static scope id but the runtime scopeId of the enclosing scope and is of the form
     * 'scopeVarId' + , + 'iterationCount'. ex: '1000002,2'
     */
    public RuntimeVariableImpl(RVariable variable, BPELProcessInstance instance, String scopeGuid) {
        mInstance = instance;
        mVarDef = variable;
        mScopeGuid = scopeGuid;
        // default values not given at construction, only prior to evaluation
    }
    
    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimeVariable#isInserted()
     */
    public boolean isInserted() {
        return mIsInserted;
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimeVariable#markInserted()
     */
    public void markInserted() {
        mIsInserted = true;
    }

    /**
     * Please refer http://wiki.open-esb.java.net/Wiki.jsp?page=ScalabilitySupport
     * for various status for update of variable. 
     * 
     * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimeVariable#setWSMessage(
     *      com.sun.jbi.engine.bpel.core.bpms.bpel.runtime.WSMessage)
     */
    public void setWSMessage(WSMessage msg) {
		synchronized (scalabilityLock) {
			if (!isPersisted() && isPassivated()) {
				/* Scenario 1 (FFT) and 3 (TFT) for update */
				deletePassivatedVariable();
			}
			if (mInstance != null) {
				// TODO bpel core junits are using this api directly,
				// hence null check is reqd. fix this.
				mInstance.markVarsPassivated(false);
			}
			
			mWSMsg = msg;
			
			/* Scenario 2 (TTT) */
			mIsPersisted = false;
            mIsPassivated = false;
		}
    }



	/**
	 * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimeVariable#getWSMessage()
	 */
    public WSMessage getWSMessage() {
		synchronized (scalabilityLock) {
			if (isPassivated()) {
				loadPassivatedVariable();
			}
		}

		return mWSMsg;
	}

    /**
	 * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimeVariable#getVariableDef()
	 */
    public RVariable getVariableDef() {
        return mVarDef;
    }

    public void setXSDVariableData(Object data) {
        synchronized (scalabilityLock) {
            if (data == null) {
                mXsdData = null;
                return;
            }
            if (mVarDef.isBoolean()) {
        	/*
        	 * Bug 648 https://open-jbi-components.dev.java.net/issues/show_bug.cgi?id=648
        	 * The InfoSetUtil.booleanValue(data) is removed here and Boolean.valueOf(data)
        	 * is used to return the correct boolean string assignments. Using the former
        	 * would result is a value of "false" being returned and "true" and that is not
        	 * what is the desired result. 
        	 */
        	String test = null;
        	if (data instanceof Node) {
        	    test = ((Node) data).getTextContent().trim();
        	} else {
        	    test = InfoSetUtil.stringValue(data);
        	}
        	mXsdData = Boolean.valueOf(test);
            } else if (mVarDef.isNumber()) {
                if (data instanceof Node) {
                    mXsdData = Double.valueOf(((Node) data).getTextContent()
                            .trim());
                } else {
                    if (mVarDef.getXSDType().getFacet(org.apache.xmlbeans.SchemaType.FACET_FRACTION_DIGITS) != null) {
                        String fractionDigits = mVarDef.getXSDType().getFacet(org.apache.xmlbeans.SchemaType.FACET_FRACTION_DIGITS).getStringValue();
                        if (fractionDigits != null && fractionDigits.length() > 0) {
                            BigDecimal bd = new BigDecimal(data.toString());
                            mXsdData = bd.setScale(Integer.parseInt(fractionDigits), BigDecimal.ROUND_HALF_UP);
                        }
                    } else {
                        mXsdData = InfoSetUtil.number(data);
                    }
                }
            } else if (mVarDef.isString() || mVarDef.isDate() 
                    || mVarDef.isDateTime() || mVarDef.isTime()) {
                // Date, DateTime, Time will be treated as Strings, and will be 
                // converted to some Date Objects during persistence (or when ever needed). Converting and storing of 
                // these Strings into Date objects may seem useful, we need to reparse them 
                // into XSD formatted date/dateTime/Time strings during assignments. Instead of incurring
                // that additional parsing during non-persistence execution, it is incurred in the persistence
                // run. As is, this will be a small overhead in comparision with persistence overhead 
                // when persistence is turned on.
                if (data instanceof Node) {
                    mXsdData = ((Node) data).getTextContent().trim();
                } else {
                    mXsdData = InfoSetUtil.stringValue(data);
                }
            } else {
                if (data instanceof Pointer) {
                    Pointer sourcePtr = (Pointer) data;
                    data = sourcePtr.getNode();
                }
                // variable may not have been initialized
                if (mXsdData == null) {
                    if (mVarDef.getElement() != null) {
                        mXsdData = Utility.constructDocumentElement(mVarDef
                                .getElement());
                    } else {
                        mXsdData = Utility.constructDocumentElement(mVarDef
                                .getType());
                    }
                }
                Utility.updateNode((Node) mXsdData, data);
            }

            if (isPassivated() && !isPersisted()) {
                /* Scenario 1 (FFT) and 3 (TFT) for update */
                deletePassivatedVariable();
            }

            if (mInstance != null) {
                // TODO bpel core junits are using this api directly,
                // hence null check is reqd. fix this.
                mInstance.markVarsPassivated(false);
            }			

            /* Scenario 2 (TTT) */
            mIsPassivated = false;
            mIsPersisted = false;
        }
    }

    public Object getXSDVariableData() {
        if (isSimpleType()) {
            return mXsdData;
        }
		synchronized (scalabilityLock) {
			if (isPassivated()) {
				loadPassivatedVariable();
			}
		}
        return mXsdData;
    }
    
    public Object getSerializedValue() {
		synchronized (scalabilityLock) {
			if (isPassivated()) {
				loadPassivatedVariable();
			}
		}
        if (mVarDef.getWSDLMessageType() == null) {
            if (mXsdData == null) {
                return null;    // value not set
            }
            else if (isSimpleType()) {
                return mXsdData;
            } 
            else {
                String strDocument = DOMHelper.createXmlString((Node) mXsdData);
                return strDocument;
            }
        } 
        else {
            WSMessage wsMsg = getWSMessage();
            if (wsMsg == null) {
                return null;
            }
            else {
                String strDocument = DOMHelper.createXmlString(wsMsg.getElement());
                return strDocument;
            }
        }
    }

    public void setSerializedValue(Object value) {
		synchronized (scalabilityLock) {
			markInserted();
			setPersisted();
			setValue(value);
		}
    }
    
    private void setValue(Object value) {
        if (value instanceof Reader) {
            if (mVarDef.getWSDLMessageType() == null) {
                Document doc = DOMHelper.readDocument((Reader) value);
                mXsdData = doc.getDocumentElement();
            } else {
                mWSMsg = Utility.getWSMessage(getVariableDef(), (Reader) value);
            }
        } else {
            // TODO as part of converting every simple type into a String type, left the
            // boolean and numeric out of the scope for first iteration.
            if (mVarDef.isBoolean()) {
                mXsdData = (value.equals("true")) ? true : false; 
            } else if (mVarDef.isNumber()) {
            		mXsdData = Double.parseDouble(value.toString());
            } else if (mVarDef.isString()) {
            	/*
            	 * Bug 550: persistence of an empty string in oracle DB
            	 * is treated as a <null>. An entry in the SimpleVariable
            	 * table means that there is a value, in this case its an
            	 * empty string.
            	 */
            	if (value == null) {
            		mXsdData = RuntimeVariableImpl.EMPTY_STRING;
            	} else {
            		mXsdData = value;
            	}
            } else {
                mXsdData = value;
            }
        }
    }
       
    /** @see java.lang.Object#toString() */
    public String toString() {
        if (getVariableDef() == null) return "RuntimeVariable-???";
        return "RuntimeVariable-"+ String.valueOf(getVariableDef().getUniqueId());
    }
    
    public void setPersisted() {
        this.mIsPersisted = true;
     }
    
    public boolean isPersisted() {
        return mIsPersisted;
    }

    public boolean isPassivated() {
        return mIsPassivated;
    }
    
    /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimeVariable#isSimpleType()
     */
    public boolean isSimpleType() {
        if (mVarDef.isDate() || mVarDef.isDateTime() || mVarDef.isTime() || mVarDef.isString()
                || mVarDef.isNumber() || mVarDef.isBoolean()) {
            return true;
        }
        return false;
    }
    
    /**
     * delete passivated variable
     */
    private void deletePassivatedVariable() {
        String stateId = mInstance.getId();
        long uniqueId = mVarDef.getUniqueId();
        mInstance.getBPELProcessManager().getMemoryMgr().deletePassivatedVariable(stateId, uniqueId, mScopeGuid);
    	this.mIsPassivated = false;
	}
    
    /**
     * load passivated variable. This will also delete the passivated
     * record in the database.
     */
    private void loadPassivatedVariable() {
		String stateId = mInstance.getId();
		long uniqueId = mVarDef.getUniqueId();
		Object value = null;
		if (isPersisted()) {
			/* Scenario 2 (TTT) for read */
			value = mInstance.getBPELProcessManager().getMemoryMgr().loadPersistedVariable(stateId, mVarDef.getName(), uniqueId, mScopeGuid);
		} else {
			/* Scenario 1 (FFT) and 3 (TFT) for read */
			value = mInstance.getBPELProcessManager().getMemoryMgr().loadPassivatedVariable(stateId, mVarDef.getName(), uniqueId, mScopeGuid);
		}
		setValue(value);
		if (mInstance.areVarsPassivated()) {
			mInstance.markVarsPassivated(false);
		}
		this.mIsPassivated = false;
	}
    
    /**
	 * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimeVariable#getScopeGuid()
	 */
    public String getScopeGuid() {
    	return mScopeGuid;
    }

	/* (non-Javadoc)
	 * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimeVariable#passivateVariable()
	 */
	public void passivateVariable() {
		synchronized (scalabilityLock) {
			if ((mWSMsg == null && mXsdData == null) || isPassivated()) {
				/* init condition (mWSMsg == null && mXsdData == null)
				 * for isPassivated() following are covered
				 * Scenario 2 (FFT)
				 * Scenario 3 (TFT) 
				 * Scenario 6 (TTT)
				 */
				return;
			}

			if ((!isInserted()&& !isPersisted()) || (isInserted() && !isPersisted())) {			
				/*Scenario 1 (FFF) or Scenario 4 (TFF)*/
				Object value = getSerializedValue();
				if (value != null) {
					char[] chars = ((String) value).toCharArray();
					mInstance.getBPELProcessManager().getMemoryMgr().passivateVariable(mInstance.getId(), mVarDef.getName(), mVarDef.getUniqueId(), chars, mScopeGuid);
				}
			} else {
				// the variable was already persisted. The following call is for logging for debugging purposes only. The reason why this is logged 
				// in ScalabilityManager is because, class level logging can be configured for ScalabilityManger.
				mInstance.getBPELProcessManager().getMemoryMgr().logPersistedVariableDereferencing(mInstance.getId(), mVarDef.getName(), mVarDef.getUniqueId(), mScopeGuid);
			}
			// (isPersisted() && isPassivated() /*Scenario 5 (TTF)*/
			this.mWSMsg = null;
			this.mXsdData = null;
			this.mIsPassivated = true;
		}
	}
}
