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
 * @(#)OperationMetaData.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.ldapbc;

import com.sun.jbi.ldapbc.extensions.LDAPOperation;
import com.sun.jbi.ldapbc.extensions.LDAPOperationOutput;

import com.sun.jbi.ldapbc.update.UpdateBean;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import javax.naming.directory.SearchControls;
import javax.wsdl.BindingOperation;

/**
 *
 * To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public class OperationMetaData {

    private LDAPOperation mLDAPOperation;
    private LDAPOperationOutput mLDAPOperationOutput;
    private BindingOperation mBindingOperation;
    private Map mQueryAttributes;
    private int recordsPerPage;
    private String dn;
    private SearchControls mSearchControl;
    private String referral;
    private Map mUpdateAttributes;
    private List<String> addedReturnAttrs = new ArrayList<String>();
    private String sortByAttribute;
    private String sortByType;
    private boolean isConnectionRecreate = false;
	private Integer retrycount;
	private Integer retryInterval;
	private Boolean allowConnectionPooling;
	private Integer connPoolPrefSize;
	private Integer connPoolMaxSize;
	private Integer connPoolMaxIdleTime;
	private String connPoolProtocol;
	private String connPoolAuthentication;

    public void addUpdateAttribute(String name, UpdateBean bean) {
        if (mUpdateAttributes == null) {
            mUpdateAttributes = new HashMap();
        }
        mUpdateAttributes.put(name, bean);
    }

    public OperationMetaData() {
    }

    public void setReferral(String referral) {
        this.referral = referral;
    }

    public String getReferral() {
        return referral;
    }

	public int getRetryCount(){
		return retrycount;
	}

	public void setRetryCount(Integer val){
		retrycount = val;
	}

	public int getRetryInterval(){
		return retryInterval;
	}

	public void setRetryInterval(Integer val){
		retryInterval = val;
	}

    public void setSearchControls(SearchControls control) {
        mSearchControl = control;
    }

    public SearchControls getSearchControls() {
        return mSearchControl;
    }

    public void setSearchDN(String dn) {
        this.dn = dn;
    }

    public String getSearchDN() {
        return dn;
    }
    
    /**
     * This method returns whether Connection Pooling is enabled or not
     * @returns Boolean
     */    
	public Boolean getAllowConnectionPooling(){
		return allowConnectionPooling;
	}

    /**
	 * This method sets whether Connection Pooling is enabled or not
     * @param Boolean
     */
	public void setAllowConnectionPooling(Boolean connPool){
		allowConnectionPooling = connPool;
	}    

    /**
     * This method returns Connection Pool Minimum Size
     * @returns Integer
     */
	public Integer getConnectionPoolPrefSize(){
		return connPoolPrefSize;
	}

    /**
	 * This method sets Connection Pool Minimum Size
     * @param Integer
     */	
	public void setConnectionPoolPrefSize(Integer prefSize){
		connPoolPrefSize = prefSize;
	}    	
	
    /**
     * This method returns Connection Pool Maximum Size
     * @returns Integer
     */	
	public Integer getConnectionPoolMaxSize(){
		return connPoolMaxSize;
	}
	
    /**
	 * This method sets Connection Pool Maximum Size
     * @param Integer
     */	
	public void setConnectionPoolMaxSize(Integer maxSize){
		connPoolMaxSize = maxSize;
	}  	
	
    /**
     * This method returns Connection Pool Maximum Idle Timeout 
     * @returns Integer
     */	
	public Integer getConnectionMaxIdleTimeout(){
		return connPoolMaxIdleTime;
	}
	
    /**
	 * This method sets Connection Pool Maximum Idle Timeout
     * @param Integer
     */	
	public void setConnectionMaxIdleTimeout(Integer idleTime){
		connPoolMaxIdleTime = idleTime;
	}  		
	
    /**
     * This method returns Connection Pool Protocol
     * @returns String
     */	
	public String getConnectionProtocol(){
		return connPoolProtocol;
	}
	
    /**
	 * This method sets Connection Pool Protocol
     * @param String
     */	
	public void setConnectionProtocol(String protocol){
		connPoolProtocol = protocol;
	}  		
	
    /**
     * This method returns Connection Pool Authentication 
     * @returns String
     */	
	public String getConnectionAuthentication(){
		return connPoolAuthentication;
	}
	
    /**
	 * This method sets Connection Pool Authentication
     * @param String
     */	
	public void setConnectionAuthentication(String auth){
		connPoolAuthentication = auth;
	}  		

    public void addAttribute(int positionIndex, int bracketDepth, int bracketBeginDepth,
            int bracketEndDepth, String logicOp, String compareOp, String attrName) {
        QueryAttributeBean bean = new QueryAttributeBean(positionIndex, bracketDepth, bracketBeginDepth,
                bracketEndDepth, logicOp, compareOp);
        if (mQueryAttributes == null) {
            mQueryAttributes = new HashMap();
        }
        mQueryAttributes.put(attrName, bean);
    }

    /**
     *
     * @param ldapOperation
     */
    public void setLDAPOperation(final LDAPOperation ldapOperation) {
        mLDAPOperation = ldapOperation;
    }

    /**
     *
     * @return
     */
    public LDAPOperation getLDAPOperation() {
        return mLDAPOperation;
    }

    /**
     *
     * @param operationOutput
     */
    public void setLDAPOperationOutput(final LDAPOperationOutput operationOutput) {
        mLDAPOperationOutput = operationOutput;
    }

    /**
     *
     * @return
     */
    public LDAPOperationOutput getLDAPOperationOutput() {
        return mLDAPOperationOutput;
    }

    /**
     *
     * @param bindingOperation
     */
    public void setBindingOperation(final BindingOperation bindingOperation) {
        mBindingOperation = bindingOperation;
    }

    /**
     *
     * @return
     */
    public BindingOperation getBindingOperation() {
        return mBindingOperation;
    }

    /**
     * 
     * @return
     */
    public Map getMQueryAttributes() {
        return mQueryAttributes;
    }

    /**
     * 
     * @param queryAttributes
     */
    public void setMQueryAttributes(Map queryAttributes) {
        mQueryAttributes = queryAttributes;
    }

    public Map getMUpdateAttributes() {
        return mUpdateAttributes;
    }

    public void setMUpdateAttributes(Map updateAttributes) {
        mUpdateAttributes = updateAttributes;
    }

    public List<String> getAddedReturnAttrs() {
        return addedReturnAttrs;
    }

    public void setAddedReturnAttrs(List<String> addedReturnAttrs) {
        this.addedReturnAttrs = addedReturnAttrs;
    }

    public int getRecordsPerPage() {
        return recordsPerPage;
    }

    public void setRecordsPerPage(int recordsPerPage) {
        this.recordsPerPage = recordsPerPage;
    }

    public String getSortByAttribute() {
        return sortByAttribute;
    }

    public void setSortByAttribute(String sortByAttribute) {
        this.sortByAttribute = sortByAttribute;
    }

    public String getSortByType() {
        return sortByType;
    }

    public void setSortByType(String sortByType) {
        this.sortByType = sortByType;
    }

    public boolean isIsConnectionRecreate() {
        return isConnectionRecreate;
    }

    public void setIsConnectionRecreate(boolean isConnectionRecreate) {
        this.isConnectionRecreate = isConnectionRecreate;
    }
}
