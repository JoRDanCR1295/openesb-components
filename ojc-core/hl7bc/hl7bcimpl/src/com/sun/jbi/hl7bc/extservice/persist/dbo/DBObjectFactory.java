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
 * @(#)DBObjectFactory.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.hl7bc.extservice.persist.dbo;

//import com.sun.jbi.hl7bc.extservice.persist.dbo.impl.AcknowledgmentDBOImpl;
import com.sun.jbi.hl7bc.extservice.persist.dbo.impl.HL7MessageLogDBOImpl;
import com.sun.jbi.hl7bc.extservice.persist.dbo.impl.SequenceNumDBOImpl;
import com.sun.jbi.hl7bc.extservice.persist.dbo.impl.JournalHL7MessageLogDBOImpl;

import java.sql.ResultSet;
import java.sql.SQLException;

import java.util.ArrayList;
import java.util.List;

/**
 *
 * @author S. Nageswara Rao
 */
public class DBObjectFactory {
    private static DBObjectFactory mSingleton;

    private int mDBType;

    private DBObjectFactory(int dbType) {
        mDBType = dbType;
    }

    /**
     * get DBObject factory
     *
     * @param dbType database type
     *
     * @return DBObjectFactory DBObject factory
     */
    public static DBObjectFactory getDBObjectFactory(int dbType) {
        if (mSingleton == null) {
            mSingleton = new DBObjectFactory(dbType);
        }

        return mSingleton;
    }

    /**
     * creates acknowledgment DBObject
     *
     * @param mcid message control id
     * @param message hl7 acknowledgment 
     * @return AcknowledgmentDBO acknowledgment DBObject
     */
    /* public AcknowledgmentDBO createAcknowledgmentDBO(String mcid, String message) {
        return new AcknowledgmentDBOImpl(mDBType, mcid, message);
    }*/

    /**
     * creates SequenceNum DBObject
     *
     * @param queryString Unique ID for the records in EXPSEQUENCENO Table
     * @param esn expected sequence number
     * @param esnState expected sequence number state
     * @return SequenceNumDBO SequenceNum DBObject
     */
    public SequenceNumDBO createSequenceNumDBO(String queryString, int esn, String esnState) {
        return new SequenceNumDBOImpl(mDBType, queryString, esn, esnState);
    }

    /**
     * creates SequenceNum DBObject
     *
     * @param queryString Unique ID for the records in EXPSEQUENCENO Table
     * @return SequenceNumDBO SequenceNum DBObject
     */
    public SequenceNumDBO createSequenceNumDBO(String queryString) {
        return new SequenceNumDBOImpl(mDBType, queryString);
    }
    
    public HL7MessageLogDBO createHL7MessageLogDBO(){
    	return new HL7MessageLogDBOImpl(mDBType);
    }
    
    public HL7MessageLogDBO createHL7MessageLogDBO(String messageId, String applicationId){
    	return new HL7MessageLogDBOImpl(mDBType, messageId, applicationId);
    }
    public JournalHL7MessageLogDBO createJournalHL7MessageLogDBO(){
    	return new JournalHL7MessageLogDBOImpl(mDBType);
    }
    
    public JournalHL7MessageLogDBO createJournalHL7MessageLogDBO(String messageControlId, String applicationId){
    	return new JournalHL7MessageLogDBOImpl(mDBType, messageControlId, applicationId);
    }

    /**
     * returns list of DBObjects from ResultSet
     *
     * @param tempObj DBObject
     * @param rs ResultSet
     *
     * @return List list of populated DBObjects
     *
     * @throws SQLException SQLException
     */
    public List populateDBO(DBObject tempObj, ResultSet rs) throws SQLException {
        List retVals = new ArrayList();
        DBObject obj = null;

        while (rs.next()) {
            obj = tempObj.createNew();
            obj.populateDBO(rs);
            retVals.add(obj);
        }

        return retVals;
    }
}
