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
 * @(#)RuntimeVariable.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.model.runtime;

import com.sun.bpel.model.meta.RVariable;


/**
 * DOCUMENT ME!
 *
 * @author Sun Microsystems
 */
public interface RuntimeVariable {
    /**
     * check if the variable is inserted
     *
     * @return boolean: if the variable is inserted, returns true;
     */
    boolean isInserted();

    /**
     * marks the variable as inserted
     */
    void markInserted();

    /**
     * sets webservice message. This is a thread safe call to support global  variables being
     * accessed by multiple threads
     *
     * @param msg webservice message
     */
    void setWSMessage(WSMessage msg);
    
    /*
     * sets the data for the container.
     */
    void setXSDVariableData(Object data);
    
    Object getXSDVariableData();

    /**
     * gets webservice message. This is a thread safe call to support global  variables being
     * accessed by multiple threads
     *
     * @return WSMessage webservice message
     */
    WSMessage getWSMessage();

    /**
     * the variable definition to which this RuntimeVariable is associated  with.
     *
     * @return variable definition
     */ 
    RVariable getVariableDef();
    
    public Object getSerializedValue();

    public void setSerializedValue(Object value);
    
    String getScopeGuid();
    
    public void setPersisted();

    /**
     * Returns if the variable data is persisted or not. This pertains to 
     * regular recover related persistece and not passivation by scalabiity thread. 
     * 
     * @return
     */
    public boolean isPersisted();

    public boolean isSimpleType();
    
    /**
     * Returns if the variable is passivated or not. If true, then the variable
     * data is persisted into database and set to null in this runtime variable 
     * object.
     *
     * @return
     */
    public boolean isPassivated();

	/**
     * The three state flags on runtime variable results in 8 possible
     * combinations. Each one of those are covered as below
     * Note: The three letters against the case be read as status for
     * inserted, persisted and passivated respectively where T = True, F = False.
     * 
     * Invalid Scenarios (FTT, FTF) Scenarios not possible, as with mIsInserted as FALSE, 
     * 						mIsInserted cannot be TRUE 
     * Scenario 1 (FFF) 	The variable was not inserted (did not hit any persistence point
     * 						at the time of scalability call
     * Scenario 2 (FFT) 	Variable was never persisted but already passivated. Hence the 
     * 						variable in memory is already null. No in-memory or DB operation required 
     * 						for this scenario. 
     * Scenario 3 (TFT) 	This indicates that the variable was previously persisted 
     * 						(as indicated by first T) and later updated (indicated by second F)
     * 						and also subsequently scalability passivated (last T). Hence at this
     * 						point the in-memory variable should already be null and there
     * 						should be two records for this variable in the DB (one regular 
     * 						persistence and other scalability passivated). This scalability
     * 						call will be no-op for memory and database also. 
     * Scenario 4 (TFF) 	This indicates that the variable was previously persisted (first F)
     * 						and subsequently updated (second F). Hence there should be one 
     * 						persistence record in the database. This scalability passivation 
     * 						call will passivate (insert) the dirty variable in the database with 
     * 						SCALABILITYPASSIVATED Flag as 'Y' resulting in two records for this 
     * 						variable
     * Scenario 5 (TTF)		Variable is already inserted (first T) and has not changed since 
     * 						last persistence point (second T). When scalability call is
     * 						made, dereference the variable in memory, no database operation 
     * 						needed. 
     * 						Note: We do not update the SCALABILITYFLAG value in the database 
     * 						for this variable, as this flag is is ONLY updated if the variable 
     * 						was dirty and its value in database was updated by the scalability thread.
     * Scenario 6 (TTT)  	No in-memory or database operation as the variable 
     * 						is both persisted and passivated and hence already null
     */
	void passivateVariable();
}
